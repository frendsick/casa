"""Op tree builder and identifier resolution for the Casa compiler."""

from pathlib import Path
from typing import assert_never

from casa.common import (
    BUILTIN_TYPES,
    GLOBAL_ENUMS,
    GLOBAL_FUNCTIONS,
    GLOBAL_SCOPE_LABEL,
    GLOBAL_STRUCTS,
    GLOBAL_TRAITS,
    GLOBAL_VARIABLES,
    INCLUDED_FILES,
    MATCH_WILDCARD,
    CasaEnum,
    Cursor,
    Delimiter,
    EnumVariant,
    Function,
    Intrinsic,
    Keyword,
    LiteralPattern,
    Location,
    Member,
    Op,
    Operator,
    OpKind,
    Parameter,
    Signature,
    Span,
    Struct,
    StructLiteral,
    StructPattern,
    Token,
    TokenKind,
    Trait,
    TraitMethod,
    Type,
    Variable,
    resolve_trait_sig,
)
from casa.error import (
    SOURCE_CACHE,
    CasaError,
    CasaErrorCollection,
    ErrorKind,
    raise_error,
)
from casa.lexer import is_negative_integer_literal, lex_file, lex_source

INTRINSIC_TO_OPKIND = {
    Intrinsic.ALLOC: OpKind.HEAP_ALLOC,
    Intrinsic.DROP: OpKind.DROP,
    Intrinsic.DUP: OpKind.DUP,
    Intrinsic.EXEC: OpKind.FN_EXEC,
    Intrinsic.LOAD8: OpKind.LOAD8,
    Intrinsic.LOAD16: OpKind.LOAD16,
    Intrinsic.LOAD32: OpKind.LOAD32,
    Intrinsic.LOAD64: OpKind.LOAD64,
    Intrinsic.OVER: OpKind.OVER,
    Intrinsic.PRINT: OpKind.PRINT,
    Intrinsic.ROT: OpKind.ROT,
    Intrinsic.STORE8: OpKind.STORE8,
    Intrinsic.STORE16: OpKind.STORE16,
    Intrinsic.STORE32: OpKind.STORE32,
    Intrinsic.STORE64: OpKind.STORE64,
    Intrinsic.SWAP: OpKind.SWAP,
    Intrinsic.TYPEOF: OpKind.TYPEOF,
    Intrinsic.SYSCALL0: OpKind.SYSCALL0,
    Intrinsic.SYSCALL1: OpKind.SYSCALL1,
    Intrinsic.SYSCALL2: OpKind.SYSCALL2,
    Intrinsic.SYSCALL3: OpKind.SYSCALL3,
    Intrinsic.SYSCALL4: OpKind.SYSCALL4,
    Intrinsic.SYSCALL5: OpKind.SYSCALL5,
    Intrinsic.SYSCALL6: OpKind.SYSCALL6,
    Intrinsic.ARGC: OpKind.ARGC,
    Intrinsic.ARGV: OpKind.ARGV,
}


def _parse_fstring_expr(
    cursor: Cursor[Token],
    function_name: str,
) -> list[Op]:
    """Parse the expression inside an f-string interpolation block."""
    expr_ops: list[Op] = []
    while (
        expr_token := cursor.pop()
    ) and expr_token.kind != TokenKind.FSTRING_EXPR_END:
        if expr_token.kind == TokenKind.FSTRING_START:
            handle_fstring(expr_token, cursor, function_name, expr_ops)
            continue
        _append_op_result(expr_ops, token_to_op(expr_token, cursor, function_name))
    return expr_ops


def handle_fstring(
    start_token: Token,
    cursor: Cursor[Token],
    function_name: str,
    ops: list[Op],
) -> None:
    """Parse an f-string into concatenated string and expression ops."""
    part_count = 0
    while token := cursor.pop():
        match token.kind:
            case TokenKind.FSTRING_TEXT:
                ops.append(Op(token.value, OpKind.PUSH_STR, token.location))
                part_count += 1
            case TokenKind.FSTRING_EXPR_START:
                expr_ops = _parse_fstring_expr(cursor, function_name)
                lambda_name = f"lambda__{function_name}_o{token.location.span.offset}"
                lambda_fn = Function(lambda_name, expr_ops, token.location)
                GLOBAL_FUNCTIONS[lambda_name] = lambda_fn
                ops.append(Op(lambda_name, OpKind.FN_PUSH, token.location))
                ops.append(Op(Intrinsic.EXEC, OpKind.FN_EXEC, token.location))
                part_count += 1
            case TokenKind.FSTRING_END:
                break
    if part_count == 0:
        ops.append(Op("", OpKind.PUSH_STR, start_token.location))
        part_count = 1
    if part_count > 1:
        ops.append(Op(part_count, OpKind.FSTRING_CONCAT, start_token.location))


def parse_ops(
    tokens: list[Token],
    resilient: bool = False,
    errors_out: list[CasaError] | None = None,
) -> list[Op]:
    """Parse a flat token list into a list of ops.

    When resilient is True, syntax errors are collected instead of raised,
    and partial results are returned. Collected errors are appended to
    errors_out if provided.
    """
    cursor = Cursor(sequence=tokens)
    ops: list[Op] = []
    while token := cursor.pop():
        try:
            if token.kind == TokenKind.FSTRING_START:
                handle_fstring(token, cursor, GLOBAL_SCOPE_LABEL, ops)
                continue
            _append_op_result(ops, token_to_op(token, cursor))
        except CasaErrorCollection as exc:
            if not resilient:
                raise
            if errors_out is not None:
                errors_out.extend(exc.errors)
    return ops


def _mark_used_and_resolve(global_function: Function) -> None:
    """Mark a function as used and resolve its identifiers if not already done."""
    if global_function.is_used:
        return
    global_function.is_used = True
    global_function.ops = resolve_identifiers(global_function.ops, global_function)


def _resolve_array_identifiers(
    items: list[Op],
    function: Function | None = None,
    errors: list[CasaError] | None = None,
) -> None:
    """Resolve identifier ops inside array literal items."""
    if errors is None:
        errors = []
    for item in items:
        if item.kind == OpKind.PUSH_ARRAY:
            _resolve_array_identifiers(item.value, function, errors)
        elif item.kind == OpKind.IDENTIFIER:
            identifier = item.value
            assert isinstance(identifier, str)
            if function and identifier in function.variables:
                item.kind = OpKind.PUSH_VARIABLE
            elif function and identifier in function.captures:
                item.kind = OpKind.PUSH_CAPTURE
            elif GLOBAL_VARIABLES.get(identifier):
                item.kind = OpKind.PUSH_VARIABLE
            else:
                errors.append(
                    CasaError(
                        ErrorKind.UNDEFINED_NAME,
                        f"Identifier `{identifier}` is not defined",
                        item.location,
                    )
                )


def _resolve_trait_method_sig(sig: Signature, type_var: str) -> Signature:
    """Create a copy of a trait method signature with self type replaced by type_var."""
    return resolve_trait_sig(sig, type_var)


def _gather_captures(
    lambda_function: Function,
    function: Function | None,
) -> None:
    """Gather captured variables for a lambda function from enclosing scopes."""
    for op in lambda_function.ops:
        if op.kind != OpKind.IDENTIFIER:
            continue
        captured = False
        if function:
            for local_variable in function.variables:
                if op.value == local_variable.name:
                    lambda_function.captures.append(local_variable)
                    captured = True
                    break
        if not captured:
            for global_variable in GLOBAL_VARIABLES.values():
                if global_variable.name == op.value:
                    lambda_function.captures.append(global_variable)
                    break


def _resolve_fn_ref(
    identifier: str,
    op: Op,
    function: Function | None,
    errors: list[CasaError],
) -> bool:
    """Resolve a function reference (&name). Returns True if handled."""
    function_name = identifier[1:]
    if not function_name:
        errors.append(
            CasaError(
                ErrorKind.SYNTAX,
                "Expected function name after `&`",
                op.location,
            )
        )
        return True
    # Check trait bound reference: &K::method
    trait_var = _resolve_trait_ref(function_name, function)
    if trait_var:
        op.kind = OpKind.PUSH_VARIABLE
        op.value = trait_var
        return True
    global_function = GLOBAL_FUNCTIONS.get(function_name)
    if not global_function:
        errors.append(
            CasaError(
                ErrorKind.UNDEFINED_NAME,
                f"Function `{function_name}` is not defined",
                op.location,
            )
        )
        return True
    op.kind = OpKind.FN_PUSH
    op.value = function_name
    _mark_used_and_resolve(global_function)
    return True


def _create_member_accessor(
    struct_name: str,
    member_name: str,
    member_type: str,
    member_index: int,
    location: Location,
    *,
    type_vars: list[str] | None = None,
) -> tuple[Function, Function]:
    """Create getter and setter functions for a struct member."""
    offset = member_index * 8

    # Parameterize the struct name when the struct has type vars
    if type_vars:
        param_struct_type = f"{struct_name}[{' '.join(type_vars)}]"
        sig_type_vars = set(type_vars)
    else:
        param_struct_type = struct_name
        sig_type_vars = set()

    # Getter
    getter_name = f"{struct_name}::{member_name}"
    getter_ops: list[Op] = [
        Op("ptr", OpKind.TYPE_CAST, location),
        Op(offset, OpKind.PUSH_INT, location),
        Op(Operator.PLUS, OpKind.ADD, location),
        Op(Intrinsic.LOAD64, OpKind.LOAD64, location),
        Op(member_type, OpKind.TYPE_CAST, location),
    ]
    getter_params = [Parameter(param_struct_type)]
    getter_signature = Signature(getter_params, [member_type], type_vars=sig_type_vars)
    getter = Function(getter_name, getter_ops, location, getter_signature)

    # Setter
    setter_name = f"{struct_name}::set_{member_name}"
    setter_ops: list[Op] = [
        Op("ptr", OpKind.TYPE_CAST, location),
        Op(offset, OpKind.PUSH_INT, location),
        Op(Operator.PLUS, OpKind.ADD, location),
        Op(Intrinsic.STORE64, OpKind.STORE64, location),
    ]
    setter_params = [Parameter(param_struct_type), Parameter(member_type)]
    setter_signature = Signature(setter_params, [], type_vars=sig_type_vars)
    setter = Function(setter_name, setter_ops, location, setter_signature)

    return getter, setter


def _resolve_trait_ref(
    name: str,
    function: Function | None,
) -> str | None:
    """Check if name is a trait-bound method call like K::hash.

    Returns the hidden variable name (e.g. __trait_K_hash) if it matches,
    or None if not a trait reference.
    """
    if not function or not function.signature:
        return None
    if "::" not in name:
        return None
    parts = name.split("::", 1)
    tv, method_name = parts[0], parts[1]
    trait_name = function.signature.trait_bounds.get(tv)
    if not trait_name:
        return None
    trait = GLOBAL_TRAITS.get(trait_name)
    if not trait:
        return None
    for method in trait.methods:
        if method.name == method_name:
            return f"__trait_{tv}_{method_name}"
    return None


def resolve_identifiers(
    ops: list[Op],
    function: Function | None = None,
) -> list[Op]:
    """Resolve raw identifiers into typed ops like FN_CALL, PUSH_VARIABLE, etc."""
    errors: list[CasaError] = []
    index = 0
    while index < len(ops):
        op = ops[index]
        index += 1
        match op.kind:
            case OpKind.ASSIGN_VARIABLE:
                variable_name = op.value
                assert isinstance(variable_name, str), "Expected variable name"
                variable = Variable(variable_name)

                # Global scope
                if not function:
                    GLOBAL_VARIABLES[variable_name] = variable
                    continue

                # Function scope: local variables shadow globals
                if variable in function.variables:
                    continue
                if GLOBAL_VARIABLES.get(variable_name):
                    continue
                function.variables.append(variable)
            case OpKind.FN_PUSH:
                function_name = op.value
                lambda_function = GLOBAL_FUNCTIONS.get(function_name)
                if not lambda_function:
                    errors.append(
                        CasaError(
                            ErrorKind.UNDEFINED_NAME,
                            f"Lambda function `{function_name}` is not defined",
                            op.location,
                        )
                    )
                    continue

                lambda_function.is_used = True
                _gather_captures(lambda_function, function)
                lambda_function.ops = resolve_identifiers(
                    lambda_function.ops, lambda_function
                )
            case OpKind.PUSH_ARRAY:
                items = op.value
                assert isinstance(items, list)
                _resolve_array_identifiers(items, function, errors)
            case OpKind.IDENTIFIER:
                identifier = op.value
                assert isinstance(identifier, str), "Expected identifier name"

                # Function reference: &name pushes a function as a value
                if identifier.startswith("&"):
                    _resolve_fn_ref(identifier, op, function, errors)
                    continue

                # Enum variant: EnumName::Variant
                if "::" in identifier:
                    parts = identifier.split("::", 1)
                    casa_enum = GLOBAL_ENUMS.get(parts[0])
                    if casa_enum:
                        variant_name = parts[1]
                        if variant_name not in casa_enum.variants:
                            errors.append(
                                CasaError(
                                    ErrorKind.UNDEFINED_NAME,
                                    f"Variant `{variant_name}` is not defined"
                                    f" in enum `{casa_enum.name}`",
                                    op.location,
                                )
                            )
                            continue
                        ordinal = casa_enum.variants.index(variant_name)
                        op.kind = OpKind.PUSH_ENUM_VARIANT
                        op.value = EnumVariant(casa_enum.name, variant_name, ordinal)
                        continue

                # Check trait bound call: K::method -> push var + exec
                trait_var = _resolve_trait_ref(identifier, function)
                if trait_var:
                    op.kind = OpKind.PUSH_VARIABLE
                    op.value = trait_var
                    exec_op = Op(Intrinsic.EXEC, OpKind.FN_EXEC, op.location)
                    ops.insert(index, exec_op)
                    index += 1
                    continue

                # Local variables shadow globals with the same name
                if function and identifier in function.variables:
                    op.kind = OpKind.PUSH_VARIABLE
                    continue
                if global_function := GLOBAL_FUNCTIONS.get(identifier):
                    op.kind = OpKind.FN_CALL
                    _mark_used_and_resolve(global_function)
                    continue
                if function and identifier in function.captures:
                    op.kind = OpKind.PUSH_CAPTURE
                    continue
                if GLOBAL_VARIABLES.get(identifier):
                    op.kind = OpKind.PUSH_VARIABLE
                    continue
                if struct := GLOBAL_STRUCTS.get(identifier):
                    op.kind = OpKind.STRUCT_NEW
                    op.value = struct
                    continue
                if casa_enum := GLOBAL_ENUMS.get(identifier):
                    op.kind = OpKind.PUSH_INT
                    op.value = len(casa_enum.variants)
                    continue

                errors.append(
                    CasaError(
                        ErrorKind.UNDEFINED_NAME,
                        f"Identifier `{identifier}` is not defined",
                        op.location,
                    )
                )
            case OpKind.MATCH_ARM:
                if isinstance(op.value, StructPattern):
                    for var_name in op.value.bindings.values():
                        variable = Variable(var_name)
                        if function:
                            if variable not in function.variables:
                                function.variables.append(variable)
                        else:
                            GLOBAL_VARIABLES[var_name] = variable
                elif isinstance(op.value, EnumVariant):
                    variant = op.value
                    # Resolve deferred enum variant (ordinal == -1)
                    if variant.enum_name and variant.ordinal == -1:
                        casa_enum = GLOBAL_ENUMS.get(variant.enum_name)
                        if not casa_enum:
                            errors.append(
                                CasaError(
                                    ErrorKind.UNDEFINED_NAME,
                                    f"Enum `{variant.enum_name}` is not defined",
                                    op.location,
                                )
                            )
                        elif variant.variant_name not in casa_enum.variants:
                            errors.append(
                                CasaError(
                                    ErrorKind.UNDEFINED_NAME,
                                    f"Variant `{variant.variant_name}` is not "
                                    f"defined in enum `{variant.enum_name}`",
                                    op.location,
                                )
                            )
                        else:
                            variant.ordinal = casa_enum.variants.index(
                                variant.variant_name
                            )
                    if variant.bindings:
                        for var_name in variant.bindings:
                            variable = Variable(var_name)
                            if function:
                                if variable not in function.variables:
                                    function.variables.append(variable)
                            else:
                                GLOBAL_VARIABLES[var_name] = variable
            case OpKind.INCLUDE_FILE:
                included_file = op.value
                assert isinstance(included_file, Path), "Expected included file path"

                if included_file not in INCLUDED_FILES:
                    INCLUDED_FILES.add(included_file)

                    # Use in-memory source if available (e.g. unsaved LSP buffer)
                    cached_source = SOURCE_CACHE.get(included_file)
                    if cached_source is not None:
                        included_tokens = lex_source(cached_source, included_file)
                    else:
                        included_tokens = lex_file(included_file)
                    included_ops = parse_ops(included_tokens)
                    ops = ops[:index] + included_ops + ops[index:]

    if errors:
        raise CasaErrorCollection(errors)

    return ops


def _is_struct_field_boundary(cursor: Cursor[Token], member_names: set[str]) -> bool:
    """Check if current position is a struct field like `field_name:`."""
    pos = cursor.position
    seq = cursor.sequence
    if pos + 1 >= len(seq):
        return False
    token = seq[pos]
    if token.kind != TokenKind.IDENTIFIER:
        return False
    if token.value not in member_names:
        return False
    return seq[pos + 1].value == ":"


def _parse_struct_field_value(
    cursor: Cursor[Token],
    struct: Struct,
    member_names: set[str],
    function_name: str,
    all_ops: list[Op],
) -> None:
    """Parse a single field's value expression in a struct literal."""
    while not cursor.is_finished():
        peek = cursor.peek()
        if not peek or peek.value == "}":
            break
        if _is_struct_field_boundary(cursor, member_names):
            break
        # Check for unknown field name (identifier followed by `:`)
        if (
            peek.kind == TokenKind.IDENTIFIER
            and peek.value not in member_names
            and cursor.position + 1 < len(cursor.sequence)
            and cursor.sequence[cursor.position + 1].value == ":"
        ):
            raise_error(
                ErrorKind.UNDEFINED_NAME,
                f"Struct `{struct.name}` has no field `{peek.value}`",
                peek.location,
            )
        value_token = cursor.pop()
        assert value_token is not None
        if value_token.kind == TokenKind.FSTRING_START:
            handle_fstring(value_token, cursor, function_name, all_ops)
            continue
        _append_op_result(all_ops, token_to_op(value_token, cursor, function_name))


def _parse_struct_literal(
    name_token: Token,
    struct: Struct,
    cursor: Cursor[Token],
    function_name: str,
) -> list[Op]:
    """Parse a struct literal: StructName { field: value ... }."""
    expect_token(cursor, value="{")
    member_names = {m.name for m in struct.members}
    field_order: list[str] = []
    all_ops: list[Op] = []
    seen_fields: set[str] = set()

    while True:
        next_tok = cursor.peek()
        if not next_tok:
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected end of input in struct literal",
                name_token.location,
                expected="`}`",
                got="end of input",
            )
        if next_tok.value == "}":
            cursor.pop()
            break

        # Parse field name
        field_token = expect_token(cursor, kind=TokenKind.IDENTIFIER)
        if field_token.value not in member_names:
            raise_error(
                ErrorKind.UNDEFINED_NAME,
                f"Struct `{struct.name}` has no field `{field_token.value}`",
                field_token.location,
            )
        if field_token.value in seen_fields:
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Duplicate field `{field_token.value}` in struct literal",
                field_token.location,
            )
        seen_fields.add(field_token.value)
        field_order.append(field_token.value)

        # Expect colon separator
        expect_token(cursor, value=":")

        # Parse value expression until next field boundary or `}`
        value_start = len(all_ops)
        _parse_struct_field_value(cursor, struct, member_names, function_name, all_ops)
        if len(all_ops) == value_start:
            raise_error(
                ErrorKind.SYNTAX,
                f"Missing value for field `{field_token.value}`" f" in struct literal",
                field_token.location,
            )

    # Validate all fields are specified
    missing = member_names - seen_fields
    if missing:
        names = ", ".join(f"`{n}`" for n in sorted(missing))
        raise_error(
            ErrorKind.TYPE_MISMATCH,
            f"Missing fields in struct literal: {names}",
            name_token.location,
        )

    literal = StructLiteral(struct, field_order)
    all_ops.append(Op(literal, OpKind.STRUCT_LITERAL, name_token.location))
    return all_ops


def _append_op_result(ops: list[Op], result: "Op | list[Op] | None") -> None:
    """Append a token_to_op result (single op, list, or None) to the ops list."""
    if result is None:
        return
    if isinstance(result, list):
        ops.extend(result)
    else:
        ops.append(result)


def token_to_op(
    token: Token,
    cursor: Cursor[Token],
    function_name: str = GLOBAL_SCOPE_LABEL,
) -> Op | list[Op] | None:
    """Convert a single token into its corresponding op."""
    assert len(TokenKind) == 12, "Exhaustive handling for `TokenKind`"

    match token.kind:
        case TokenKind.DELIMITER:
            return get_op_delimiter(token, cursor, function_name)
        case TokenKind.EOF:
            return None
        case (
            TokenKind.FSTRING_END
            | TokenKind.FSTRING_EXPR_END
            | TokenKind.FSTRING_EXPR_START
            | TokenKind.FSTRING_START
            | TokenKind.FSTRING_TEXT
        ):
            raise AssertionError(
                f"F-string token `{token.kind}` should be handled by handle_fstring"
            )
        case TokenKind.IDENTIFIER:
            next_tok = cursor.peek()
            if next_tok and next_tok.value == "{":
                struct = GLOBAL_STRUCTS.get(token.value)
                if struct:
                    return _parse_struct_literal(token, struct, cursor, function_name)
            return Op(token.value, OpKind.IDENTIFIER, token.location)
        case TokenKind.LITERAL:
            return get_op_literal(token)
        case TokenKind.INTRINSIC:
            return get_op_intrinsic(token)
        case TokenKind.KEYWORD:
            return get_op_keyword(token, cursor, function_name)
        case TokenKind.OPERATOR:
            return get_op_operator(token, cursor, function_name)
        case _:
            assert_never(token.kind)


def get_op_delimiter(
    token: Token,
    cursor: Cursor[Token],
    function_name: str,
) -> Op | None:
    """Convert a delimiter token into its op."""
    assert len(Delimiter) == 12, "Exhaustive handling for `Delimiter`"

    delimiter = Delimiter.from_str(token.value)
    match delimiter:
        case None:
            return None
        case Delimiter.ARROW:
            member = expect_token(cursor, kind=TokenKind.IDENTIFIER)
            return Op(f"set_{member.value}", OpKind.METHOD_CALL, member.location)
        case Delimiter.COLON:
            return None
        case Delimiter.COMMA:
            return None
        case Delimiter.DOT:
            method = expect_token(cursor, kind=TokenKind.IDENTIFIER)
            return Op(method.value, OpKind.METHOD_CALL, method.location)
        case Delimiter.FAT_ARROW:
            return None
        case Delimiter.HASHTAG:
            return None
        # Lambda function
        case Delimiter.OPEN_BRACE:
            cursor.position -= 1
            ops = parse_block_ops(cursor, function_name)
            lambda_name = f"lambda__{function_name}_o{token.location.span.offset}"
            lambda_function = Function(lambda_name, ops, token.location)
            GLOBAL_FUNCTIONS[lambda_name] = lambda_function
            return Op(lambda_name, OpKind.FN_PUSH, token.location)
        case Delimiter.CLOSE_BRACE:
            return None
        case Delimiter.OPEN_BRACKET:
            cursor.position -= 1
            return get_op_array(cursor)
        case Delimiter.CLOSE_BRACKET:
            return None
        case Delimiter.OPEN_PAREN:
            cursor.position -= 1
            return get_op_type_cast(cursor)
        case Delimiter.CLOSE_PAREN:
            return None
        case _:
            assert_never(delimiter)


def expect_delimiter(cursor: Cursor[Token], expected: Delimiter) -> Delimiter | None:
    """Consume and return the expected delimiter, or None if not found."""
    token = cursor.peek()
    if not token:
        return None

    delimiter = Delimiter.from_str(token.value)
    if delimiter != expected:
        return None

    cursor.position += 1
    return delimiter


def parse_block_ops(cursor: Cursor[Token], function_name: str) -> list[Op]:
    """Parse a brace-delimited block of ops."""
    open_brace = cursor.pop()
    if not open_brace or open_brace.value != "{":
        loc = open_brace.location if open_brace else None
        raise_error(
            ErrorKind.UNEXPECTED_TOKEN,
            "Unexpected token",
            loc,
            expected="`{`",
            got=f"`{open_brace.value}`" if open_brace else "nothing",
        )

    ops: list[Op] = []
    while token := cursor.pop():
        if token.value == "}":
            return ops
        if token.kind == TokenKind.FSTRING_START:
            handle_fstring(token, cursor, function_name, ops)
            continue
        _append_op_result(ops, token_to_op(token, cursor, function_name))
    raise_error(ErrorKind.UNMATCHED_BLOCK, "Unclosed block", open_brace.location)


def get_op_array(cursor: Cursor[Token]) -> Op:
    """Parse an array literal from bracket-delimited tokens."""
    open_bracket = expect_token(cursor, value="[")

    array_items = []
    while not expect_delimiter(cursor, Delimiter.CLOSE_BRACKET):
        next_token = cursor.peek()
        if next_token and next_token.value == "[":
            array_items.append(get_op_array(cursor))
            expect_delimiter(cursor, Delimiter.COMMA)
            continue

        value_token = cursor.pop()
        if not value_token or value_token.kind not in (
            TokenKind.LITERAL,
            TokenKind.IDENTIFIER,
        ):
            got = f"`{value_token.kind.name}`" if value_token else "nothing"
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected token in array literal",
                value_token.location if value_token else None,
                expected="literal or identifier",
                got=got,
            )
        item_op = token_to_op(value_token, cursor)
        assert isinstance(item_op, Op), "Array item token always produces an Op"
        array_items.append(item_op)
        expect_delimiter(cursor, Delimiter.COMMA)

    return Op(array_items, OpKind.PUSH_ARRAY, open_bracket.location)


def get_op_intrinsic(token: Token) -> Op:
    """Convert an intrinsic token into its op."""
    assert len(INTRINSIC_TO_OPKIND) == len(
        Intrinsic
    ), "Exhaustive handling for `Intrinsic`"

    intrinsic = Intrinsic.from_lowercase(token.value)
    assert intrinsic, f"Token `{token.value}` is not an intrinsic"
    return Op(intrinsic, INTRINSIC_TO_OPKIND[intrinsic], token.location)


def _require_global_scope(function_name: str, what: str, location: Location) -> None:
    """Raise an error if not in global scope."""
    if function_name != GLOBAL_SCOPE_LABEL:
        raise_error(
            ErrorKind.INVALID_SCOPE,
            f"{what} should be defined in the global scope",
            location,
        )


def _handle_keyword_fn(token: Token, cursor: Cursor[Token], function_name: str) -> None:
    """Handle the `fn` keyword: parse and register a function definition."""
    _require_global_scope(function_name, "Functions", token.location)
    cursor.position -= 1
    function = parse_function(cursor)
    if GLOBAL_FUNCTIONS.get(function.name):
        raise_error(
            ErrorKind.DUPLICATE_NAME,
            f"Identifier `{function.name}` is already defined",
            function.location,
        )
    GLOBAL_FUNCTIONS[function.name] = function


def parse_enum(cursor: Cursor[Token]) -> CasaEnum:
    """Parse an enum definition."""
    expect_token(cursor, value="enum")
    name = expect_token(cursor, kind=TokenKind.IDENTIFIER)

    # Parse optional type parameters: enum Option[T] { ... }
    type_vars: list[str] = []
    next_token = cursor.peek()
    if next_token and next_token.value == "[":
        type_vars, trait_bounds = parse_type_vars(cursor)
        if trait_bounds:
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Trait bounds are not allowed on enum definitions",
                name.location,
            )

    expect_token(cursor, value="{")
    variants: list[str] = []
    variant_locations: dict[str, Location] = {}
    variant_types: dict[str, list[str]] = {}
    while True:
        variant_token = expect_token(cursor)
        if variant_token.value == "}":
            break
        if variant_token.kind != TokenKind.IDENTIFIER:
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected token in enum definition",
                variant_token.location,
                expected="identifier",
                got=f"`{variant_token.value}`",
            )
        if variant_token.value in variants:
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Duplicate variant `{variant_token.value}` in enum",
                variant_token.location,
            )
        variants.append(variant_token.value)
        variant_locations[variant_token.value] = variant_token.location

        # Parse optional inner types: Variant(type1 type2 ...)
        inner_types: list[str] = []
        peeked = cursor.peek()
        if peeked and peeked.value == "(":
            cursor.pop()  # consume (
            while True:
                inner_tok = cursor.peek()
                if not inner_tok:
                    raise_error(
                        ErrorKind.UNEXPECTED_TOKEN,
                        "Unexpected end of input in variant inner types",
                        variant_token.location,
                        expected="`)`",
                        got="end of input",
                    )
                if inner_tok.value == ")":
                    cursor.pop()  # consume )
                    break
                inner_types.append(parse_type(cursor))
            if not inner_types:
                raise_error(
                    ErrorKind.SYNTAX,
                    "Variant inner types cannot be empty."
                    " Remove the parentheses for a variant without inner values",
                    variant_token.location,
                )
        variant_types[variant_token.value] = inner_types

    if not variants:
        raise_error(
            ErrorKind.SYNTAX,
            "Enum must have at least one variant",
            name.location,
        )
    return CasaEnum(
        name.value,
        variants,
        name.location,
        variant_locations,
        variant_types=variant_types,
        type_vars=type_vars,
    )


def _handle_keyword_enum(
    token: Token, cursor: Cursor[Token], function_name: str
) -> None:
    """Handle the `enum` keyword: parse and register an enum definition."""
    _require_global_scope(function_name, "Enums", token.location)
    cursor.position -= 1
    casa_enum = parse_enum(cursor)
    if casa_enum.name in GLOBAL_ENUMS:
        raise_error(
            ErrorKind.DUPLICATE_NAME,
            f"Enum `{casa_enum.name}` is already defined",
            casa_enum.location,
        )
    if casa_enum.name in GLOBAL_STRUCTS:
        raise_error(
            ErrorKind.DUPLICATE_NAME,
            f"Identifier `{casa_enum.name}` is already defined as a struct",
            casa_enum.location,
        )
    GLOBAL_ENUMS[casa_enum.name] = casa_enum


def _is_match_arm_boundary(cursor: Cursor[Token]) -> bool:
    """Check if current position looks like a match arm pattern (Ident => or _ =>)."""
    pos = cursor.position
    seq = cursor.sequence
    if pos + 1 >= len(seq):
        return False
    token = seq[pos]
    if token.value == MATCH_WILDCARD and seq[pos + 1].value == "=>":
        return True
    if token.kind == TokenKind.LITERAL and seq[pos + 1].value == "=>":
        return True
    if token.kind != TokenKind.IDENTIFIER:
        return False
    next_value = seq[pos + 1].value
    if next_value == "=>":
        return True
    if next_value == "{" and token.value in GLOBAL_STRUCTS:
        return True
    # EnumName::Variant(bindings) => pattern
    if next_value == "(" and "::" in token.value:
        scan = pos + 2
        while scan < len(seq) and seq[scan].value != ")":
            scan += 1
        if scan + 1 < len(seq) and seq[scan + 1].value == "=>":
            return True
    return False


def _parse_struct_match_pattern(
    name_token: Token, cursor: Cursor[Token]
) -> StructPattern:
    """Parse a struct destructuring pattern: StructName { field: var ... } =>."""
    expect_token(cursor, value="{")
    struct = GLOBAL_STRUCTS.get(name_token.value)
    if not struct:
        raise_error(
            ErrorKind.UNDEFINED_NAME,
            f"Struct `{name_token.value}` is not defined",
            name_token.location,
        )
    member_names = {m.name for m in struct.members}
    bindings: dict[str, str] = {}

    while True:
        next_tok = cursor.peek()
        if not next_tok:
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected end of input in struct pattern",
                name_token.location,
                expected="`}`",
                got="end of input",
            )
        if next_tok.value == "}":
            cursor.pop()
            break

        field_token = expect_token(cursor, kind=TokenKind.IDENTIFIER)
        if field_token.value not in member_names:
            raise_error(
                ErrorKind.UNDEFINED_NAME,
                f"Struct `{struct.name}` has no field `{field_token.value}`",
                field_token.location,
            )
        if field_token.value in bindings:
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Duplicate field `{field_token.value}` in struct pattern",
                field_token.location,
            )

        expect_token(cursor, value=":")
        var_token = expect_token(cursor, kind=TokenKind.IDENTIFIER)
        bindings[field_token.value] = var_token.value

    expect_token(cursor, value="=>")
    return StructPattern(name_token.value, bindings)


def _parse_literal_match_pattern(token: Token) -> LiteralPattern:
    """Parse a literal value into a LiteralPattern for a match arm."""
    value = token.value
    if value == "true":
        return LiteralPattern(True, "bool", "true")
    if value == "false":
        return LiteralPattern(False, "bool", "false")
    if value.isdigit() or is_negative_integer_literal(value):
        return LiteralPattern(int(value), "int", value)
    if value.startswith("'") and value.endswith("'") and len(value) == 3:
        return LiteralPattern(ord(value[1]), "char", value)
    if value.startswith('"') and value.endswith('"'):
        return LiteralPattern(value[1:-1], "str", value)
    raise_error(
        ErrorKind.UNEXPECTED_TOKEN,
        f"Unsupported literal in match arm: `{value}`",
        token.location,
    )


def _handle_keyword_match(
    token: Token, cursor: Cursor[Token], function_name: str
) -> list[Op]:
    """Handle the `match` keyword: parse a match block into ops."""
    ops: list[Op] = []
    ops.append(Op(Keyword.MATCH, OpKind.MATCH_START, token.location))

    # Parse arms until `end`
    while True:
        arm_token = expect_token(cursor)
        if arm_token.value == "end":
            ops.append(Op(Keyword.END, OpKind.MATCH_END, arm_token.location))
            break

        # Wildcard arm: _ =>
        if arm_token.value == MATCH_WILDCARD:
            expect_token(cursor, value="=>")
            variant = EnumVariant(None, MATCH_WILDCARD, -1)
            ops.append(Op(variant, OpKind.MATCH_ARM, arm_token.location))
        elif arm_token.kind == TokenKind.LITERAL:
            expect_token(cursor, value="=>")
            literal_pattern = _parse_literal_match_pattern(arm_token)
            ops.append(Op(literal_pattern, OpKind.MATCH_ARM, arm_token.location))
        elif arm_token.kind != TokenKind.IDENTIFIER:
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Expected match pattern or `_`",
                arm_token.location,
                expected="enum variant, struct pattern, literal, or `_`",
                got=f"`{arm_token.value}`",
            )
        elif (next_token := cursor.peek()) and next_token.value == "{":
            # Struct pattern: StructName { field: var ... } =>
            pattern = _parse_struct_match_pattern(arm_token, cursor)
            ops.append(Op(pattern, OpKind.MATCH_ARM, arm_token.location))
        elif "::" not in arm_token.value:
            # Bare identifier, defer validation to the type checker
            expect_token(cursor, value="=>")
            variant = EnumVariant(None, arm_token.value, -1)
            ops.append(Op(variant, OpKind.MATCH_ARM, arm_token.location))
        else:
            # Store raw enum::variant, defer resolution to resolve_identifiers
            parts = arm_token.value.split("::", 1)
            enum_name, variant_name = parts[0], parts[1]

            # Parse optional bindings: EnumName::Variant(binding1 binding2) =>
            bindings: list[str] = []
            peeked = cursor.peek()
            if peeked and peeked.value == "(":
                cursor.pop()  # consume (
                while True:
                    inner_tok = cursor.peek()
                    if not inner_tok:
                        raise_error(
                            ErrorKind.UNEXPECTED_TOKEN,
                            "Unexpected end of input in match arm bindings",
                            arm_token.location,
                            expected="`)`",
                            got="end of input",
                        )
                    if inner_tok.value == ")":
                        cursor.pop()  # consume )
                        break
                    binding_tok = expect_token(cursor, kind=TokenKind.IDENTIFIER)
                    bindings.append(binding_tok.value)

            expect_token(cursor, value="=>")
            variant = EnumVariant(enum_name, variant_name, -1, bindings)
            ops.append(Op(variant, OpKind.MATCH_ARM, arm_token.location))

        # Parse arm body
        peeked = cursor.peek()
        if peeked and peeked.value == "{":
            # Brace-delimited arm body for multiline arms
            ops.extend(parse_block_ops(cursor, function_name))
        else:
            # Single-line arm body: parse until next arm or `end`
            while not cursor.is_finished():
                if _is_match_arm_boundary(cursor):
                    break
                next_token = cursor.peek()
                if next_token and next_token.value == "end":
                    break
                body_token = cursor.pop()
                if body_token is None:
                    break
                if body_token.kind == TokenKind.FSTRING_START:
                    handle_fstring(body_token, cursor, function_name, ops)
                    continue
                _append_op_result(ops, token_to_op(body_token, cursor, function_name))

    return ops


def _handle_keyword_struct(
    token: Token, cursor: Cursor[Token], function_name: str
) -> None:
    """Handle the `struct` keyword: parse and register a struct definition."""
    _require_global_scope(function_name, "Structs", token.location)
    cursor.position -= 1
    struct = parse_struct(cursor)
    GLOBAL_STRUCTS[struct.name] = struct
    for member in struct.members:
        if member.name in GLOBAL_FUNCTIONS:
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Function `{member.name}` already exists",
                struct.location,
            )


def _handle_keyword_trait(
    token: Token, cursor: Cursor[Token], function_name: str
) -> None:
    """Handle the `trait` keyword: parse and register a trait definition."""
    _require_global_scope(function_name, "Traits", token.location)
    cursor.position -= 1
    trait = parse_trait(cursor)
    if trait.name in GLOBAL_TRAITS:
        raise_error(
            ErrorKind.DUPLICATE_NAME,
            f"Trait `{trait.name}` is already defined",
            trait.location,
        )
    GLOBAL_TRAITS[trait.name] = trait


def _handle_keyword_include(token: Token, cursor: Cursor[Token]) -> Op:
    """Handle the `include` keyword: parse the include path."""
    string_literal = expect_token(cursor, kind=TokenKind.LITERAL)
    literal_op = get_op_literal(string_literal)
    if literal_op.kind != OpKind.PUSH_STR:
        raise_error(
            ErrorKind.UNEXPECTED_TOKEN,
            "Unexpected token for include path",
            string_literal.location,
            expected="string literal",
            got=f"`{string_literal.value}`",
        )
    assert isinstance(literal_op.value, str), "Included file path"
    included_path = Path(literal_op.value)
    if not included_path.is_absolute():
        token_path = token.location.file.parent
        included_path = token_path / included_path
    return Op(included_path.resolve(), OpKind.INCLUDE_FILE, string_literal.location)


SIMPLE_KEYWORD_OPS = {
    Keyword.BREAK: OpKind.WHILE_BREAK,
    Keyword.CONTINUE: OpKind.WHILE_CONTINUE,
    Keyword.DO: OpKind.WHILE_CONDITION,
    Keyword.DONE: OpKind.WHILE_END,
    Keyword.ELIF: OpKind.IF_ELIF,
    Keyword.ELSE: OpKind.IF_ELSE,
    Keyword.FI: OpKind.IF_END,
    Keyword.IF: OpKind.IF_START,
    Keyword.RETURN: OpKind.FN_RETURN,
    Keyword.THEN: OpKind.IF_CONDITION,
    Keyword.WHILE: OpKind.WHILE_START,
}


def get_op_keyword(
    token: Token,
    cursor: Cursor[Token],
    function_name: str,
) -> Op | list[Op] | None:
    """Convert a keyword token into its op, parsing any nested blocks."""
    assert len(Keyword) == 19, "Exhaustive handling for `Keyword"

    keyword = Keyword.from_lowercase(token.value)
    assert keyword, f"Token `{token.value}` is not a keyword"

    if keyword in SIMPLE_KEYWORD_OPS:
        return Op(keyword, SIMPLE_KEYWORD_OPS[keyword], token.location)

    match keyword:
        case Keyword.END:
            raise_error(
                ErrorKind.UNMATCHED_BLOCK,
                "`end` without matching `match`",
                token.location,
            )
        case Keyword.ENUM:
            _handle_keyword_enum(token, cursor, function_name)
            return None
        case Keyword.FN:
            _handle_keyword_fn(token, cursor, function_name)
            return None
        case Keyword.IMPL:
            _require_global_scope(
                function_name, "Implementation blocks", token.location
            )
            cursor.position -= 1
            parse_impl_block(cursor)
            return None
        case Keyword.INCLUDE:
            return _handle_keyword_include(token, cursor)
        case Keyword.MATCH:
            return _handle_keyword_match(token, cursor, function_name)
        case Keyword.STRUCT:
            _handle_keyword_struct(token, cursor, function_name)
            return None
        case Keyword.TRAIT:
            _handle_keyword_trait(token, cursor, function_name)
            return None
        case _:
            raise AssertionError(f"Unhandled keyword: {keyword}")


def parse_type(cursor: Cursor[Token]) -> Type:
    """Parse a type, potentially parameterized like array[int] or fn[int -> int]."""
    next_token = cursor.peek()
    if next_token and next_token.value == "fn":
        base = cursor.pop()
        assert base is not None
    else:
        base = expect_token(cursor, kind=TokenKind.IDENTIFIER)
    next_tok = cursor.peek()
    if not next_tok or next_tok.value != "[":
        return base.value
    cursor.position += 1  # consume [

    # fn types need balanced bracket tracking for inner signatures
    if base.value == "fn":
        result: list[str] = []
        depth = 0
        while token := cursor.pop():
            if token.value == "[":
                depth += 1
                result.append("[")
            elif token.value == "]":
                if depth == 0:
                    break
                depth -= 1
                result.append("]")
            else:
                if result and result[-1] not in ("[",) and token.value not in ("]",):
                    result.append(" ")
                result.append(token.value)
        return f"fn[{''.join(result)}]"

    inner_types: list[str] = []
    while True:
        next_tok = cursor.peek()
        if next_tok and next_tok.value == "]":
            cursor.pop()
            break
        inner_types.append(parse_type(cursor))
    if not inner_types:
        raise_error(
            ErrorKind.UNEXPECTED_TOKEN,
            f"Expected type parameter inside `{base.value}[...]`",
            base.location,
        )
    inner = " ".join(inner_types)
    return f"{base.value}[{inner}]"


def get_op_type_cast(cursor: Cursor[Token]) -> Op:
    """Parse a parenthesized type cast expression."""
    open_paren = expect_token(cursor, value="(")
    cast_type = parse_type(cursor)
    close_paren = expect_token(cursor, value=")")

    open_offset = open_paren.location.span.offset
    close_offset = close_paren.location.span.offset
    cast_span_length = close_offset - open_offset + 1
    location = Location(open_paren.location.file, Span(open_offset, cast_span_length))
    return Op(cast_type, OpKind.TYPE_CAST, location)


def parse_impl_block(cursor: Cursor[Token]):
    """Parse an impl block and register its methods as namespaced functions."""
    expect_token(cursor, value="impl")

    # Parse optional type parameters: impl[K: Hashable] Set[K] { ... }
    impl_type_vars: list[str] = []
    impl_trait_bounds: dict[str, str] = {}
    next_token = cursor.peek()
    if next_token and next_token.value == "[":
        impl_type_vars, impl_trait_bounds = parse_type_vars(cursor)

    impl_type = parse_type(cursor)
    # Use the base type name (without type params) for method namespacing
    impl_base_type = impl_type.split("[")[0] if "[" in impl_type else impl_type

    expect_token(cursor, value="{")
    while (fn := cursor.peek()) and fn.value == "fn":
        function = parse_function(
            cursor,
            inherited_type_vars=set(impl_type_vars) if impl_type_vars else None,
            inherited_trait_bounds=impl_trait_bounds or None,
        )
        function.name = f"{impl_base_type}::{function.name}"
        if GLOBAL_FUNCTIONS.get(function.name):
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Identifier `{function.name}` is already defined",
                function.location,
            )

        GLOBAL_FUNCTIONS[function.name] = function
    expect_token(cursor, value="}")


def parse_struct(cursor: Cursor[Token]) -> Struct:
    """Parse a struct definition and auto-generate getter and setter functions."""
    expect_token(cursor, value="struct")
    struct_name = expect_token(cursor, kind=TokenKind.IDENTIFIER)

    # Parse optional type parameters: struct Foo[T] { ... }
    # Trait bounds are not allowed on structs — they belong on impl blocks.
    type_vars: list[str] = []
    next_token = cursor.peek()
    if next_token and next_token.value == "[":
        type_vars, trait_bounds = parse_type_vars(cursor)
        if trait_bounds:
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Trait bounds are not allowed on struct definitions. "
                "Use `impl[K: Bound] StructName[K]` instead",
                struct_name.location,
            )

    members: list[Member] = []
    expect_token(cursor, value="{")
    while True:
        member_name = expect_token(cursor)
        if member_name.value == "}":
            break
        if member_name.kind != TokenKind.IDENTIFIER:
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected token in struct definition",
                member_name.location,
                expected="identifier",
                got=f"`{member_name.kind.name}`",
            )

        expect_token(cursor, value=":")
        member_type_str = parse_type(cursor)

        getter, setter = _create_member_accessor(
            struct_name.value,
            member_name.value,
            member_type_str,
            len(members),
            member_name.location,
            type_vars=type_vars or None,
        )
        for accessor in (getter, setter):
            if accessor.name in GLOBAL_FUNCTIONS:
                raise_error(
                    ErrorKind.DUPLICATE_NAME,
                    f"Function `{accessor.name}` is already defined",
                    member_name.location,
                )
            GLOBAL_FUNCTIONS[accessor.name] = accessor

        members.append(Member(member_name.value, member_type_str))

    return Struct(
        struct_name.value,
        members,
        struct_name.location,
        type_vars=type_vars,
    )


def parse_trait(cursor: Cursor[Token]) -> Trait:
    """Parse a trait definition with its method signatures."""
    expect_token(cursor, value="trait")
    trait_name = expect_token(cursor, kind=TokenKind.IDENTIFIER)

    methods: list[TraitMethod] = []
    expect_token(cursor, value="{")
    while True:
        next_tok = cursor.peek()
        if not next_tok:
            raise_error(
                ErrorKind.UNMATCHED_BLOCK,
                "Unclosed trait block",
                trait_name.location,
            )
        if next_tok.value == "}":
            cursor.pop()
            break
        if next_tok.value != "fn":
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Expected method declaration in trait",
                next_tok.location,
                expected="`fn`",
                got=f"`{next_tok.value}`",
            )
        cursor.pop()  # consume fn
        method_name = expect_token(cursor, kind=TokenKind.IDENTIFIER)
        signature = parse_trait_method_signature(cursor)
        methods.append(TraitMethod(method_name.value, signature))

    return Trait(trait_name.value, methods, trait_name.location)


def parse_trait_method_signature(cursor: Cursor[Token]) -> Signature:
    """Parse a trait method signature (no body, stops at fn or })."""
    parameters: list[Parameter] = []

    # Parse parameters, stopping at ->, fn, or }
    while name_or_type := cursor.peek():
        if name_or_type.value in ("->", "{", "fn", "}"):
            break
        cursor.pop()

        if name_or_type.kind != TokenKind.IDENTIFIER and name_or_type.value != "fn":
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected token in trait method parameter list",
                name_or_type.location,
                expected="identifier",
                got=f"`{name_or_type.kind.name}`",
            )

        next_token = cursor.peek()
        if next_token and next_token.value == ":":
            cursor.pop()
            typ = parse_type(cursor)
            parameters.append(Parameter(typ, name_or_type.value))
        else:
            cursor.position -= 1
            typ = parse_type(cursor)
            parameters.append(Parameter(typ))

    return_types: list[Type] = []
    next_token = cursor.peek()
    if next_token and next_token.value == "->":
        cursor.pop()
        while token := cursor.peek():
            if token.value in ("fn", "}"):
                break
            return_types.append(parse_type(cursor))

    return Signature(parameters, return_types)


def parse_type_vars(
    cursor: Cursor[Token],
) -> tuple[list[str], dict[str, str]]:
    """Parse bracketed type variables and optional trait bounds."""
    expect_token(cursor, value="[")
    type_vars: list[str] = []
    seen_vars: set[str] = set()
    trait_bounds: dict[str, str] = {}
    while token := cursor.pop():
        if token.value == "]":
            if not type_vars:
                raise_error(
                    ErrorKind.SYNTAX,
                    "Empty type parameter list `[]` is not allowed",
                    token.location,
                )
            return type_vars, trait_bounds
        if token.kind == TokenKind.IDENTIFIER:
            if token.value in BUILTIN_TYPES:
                raise_error(
                    ErrorKind.DUPLICATE_NAME,
                    f"Type variable `{token.value}` shadows built-in type `{token.value}`",
                    token.location,
                )
            if token.value in GLOBAL_STRUCTS:
                raise_error(
                    ErrorKind.DUPLICATE_NAME,
                    f"Type variable `{token.value}` shadows struct type `{token.value}`",
                    token.location,
                )
            if token.value in GLOBAL_ENUMS:
                raise_error(
                    ErrorKind.DUPLICATE_NAME,
                    f"Type variable `{token.value}` shadows enum type `{token.value}`",
                    token.location,
                )
            if token.value in seen_vars:
                raise_error(
                    ErrorKind.DUPLICATE_NAME,
                    f"Duplicate type variable `{token.value}`",
                    token.location,
                )
            type_vars.append(token.value)
            seen_vars.add(token.value)
            # Check for trait bound: K: Hashable
            next_tok = cursor.peek()
            if next_tok and next_tok.value == ":":
                cursor.pop()  # consume :
                trait_tok = expect_token(cursor, kind=TokenKind.IDENTIFIER)
                trait = GLOBAL_TRAITS.get(trait_tok.value)
                if not trait:
                    raise_error(
                        ErrorKind.UNDEFINED_NAME,
                        f"Trait `{trait_tok.value}` is not defined",
                        trait_tok.location,
                    )
                trait_bounds[token.value] = trait_tok.value
        elif token.value != ",":
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected token in type parameters",
                token.location,
                expected="type variable name",
                got=f"`{token.value}`",
            )
    raise_error(ErrorKind.SYNTAX, "Expected `]` to close type parameters")


def parse_function(
    cursor: Cursor[Token],
    *,
    inherited_type_vars: set[str] | None = None,
    inherited_trait_bounds: dict[str, str] | None = None,
) -> Function:
    """Parse a function definition with optional type parameters and signature."""
    expect_token(cursor, value="fn")
    name = expect_token(cursor, kind=TokenKind.IDENTIFIER)

    # Parse optional type parameters: fn name[T1 T2] or fn name[K: Hashable, V]
    type_vars: list[str] = []
    trait_bounds: dict[str, str] = {}
    next_token = cursor.peek()
    if next_token and next_token.value == "[":
        type_vars, trait_bounds = parse_type_vars(cursor)

    # Merge inherited type vars/bounds from impl block
    if inherited_type_vars:
        for tv in inherited_type_vars:
            if tv not in type_vars:
                type_vars.append(tv)
    if inherited_trait_bounds:
        trait_bounds = {**inherited_trait_bounds, **trait_bounds}

    signature = parse_signature(cursor)
    signature.type_vars = set(type_vars)
    signature.trait_bounds = trait_bounds

    ops: list[Op] = []
    variables: list[Variable] = []

    # Expand trait bounds into hidden parameters and variables.
    # For each bound (K: Hashable), add hidden fn pointer params for each
    # trait method. These are prepended to the signature so they sit on top
    # of the stack before the user-visible params.
    # Replace self type with the type variable name in signatures.
    hidden_params: list[Parameter] = []
    for tv, trait_name in trait_bounds.items():
        trait = GLOBAL_TRAITS[trait_name]
        for method in trait.methods:
            hidden_name = f"__trait_{tv}_{method.name}"
            resolved_sig = _resolve_trait_method_sig(method.signature, tv)
            hidden_type = f"fn[{resolved_sig}]"
            hidden_params.append(Parameter(hidden_type, hidden_name))
            variables.append(Variable(hidden_name, hidden_type))
            ops.append(Op(hidden_name, OpKind.ASSIGN_VARIABLE, name.location))

    signature.parameters = hidden_params + signature.parameters

    for param in signature.parameters:
        if param.name and not param.name.startswith("__trait_"):
            variables.append(Variable(param.name, param.typ))
            ops.append(Op(param.name, OpKind.ASSIGN_VARIABLE, name.location))

    ops += parse_block_ops(cursor, name.value)
    function = Function(
        name.value, ops, name.location, signature=signature, variables=variables
    )
    return function


def parse_signature(cursor: Cursor[Token]) -> Signature:
    """Parse a function signature with parameters and return types."""
    parameters = parse_parameters(cursor)
    return_types = []

    next_token = cursor.peek()
    if not next_token:
        raise_error(
            ErrorKind.UNEXPECTED_TOKEN,
            "Unexpected end of input",
            expected="`->` or `{`",
            got="nothing",
        )

    if next_token.value == "->":
        cursor.position += 1
        return_types = parse_return_types(cursor)

    return Signature(parameters, return_types)


def parse_parameters(cursor: Cursor[Token]) -> list[Parameter]:
    """Parse function parameters as name:type pairs."""
    parameters: list[Parameter] = []
    while name_or_type := cursor.pop():
        if name_or_type.value in ("->", "{"):
            cursor.position -= 1
            return parameters

        # Allow fn keyword as an unnamed parameter type
        if name_or_type.value == "fn":
            cursor.position -= 1
            typ = parse_type(cursor)
            parameters.append(Parameter(typ))
            continue

        if name_or_type.kind != TokenKind.IDENTIFIER:
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected token in parameter list",
                name_or_type.location,
                expected="identifier",
                got=f"`{name_or_type.kind.name}`",
            )

        # Parse typed parameter
        next_token = cursor.peek()
        if next_token and next_token.value == ":":
            cursor.position += 1
            typ = parse_type(cursor)
            parameters.append(Parameter(typ, name_or_type.value))
        else:
            # Unnamed parameter - might be parameterized like array[int]
            cursor.position -= 1
            typ = parse_type(cursor)
            parameters.append(Parameter(typ))
    raise_error(
        ErrorKind.UNEXPECTED_TOKEN,
        "Unexpected end of input",
        expected="`->` or block",
        got="nothing",
    )


def parse_return_types(cursor: Cursor[Token]) -> list[Type]:
    """Parse return types after the arrow in a function signature."""
    return_types: list[Type] = []
    while token := cursor.peek():
        if token.value == "{":
            return return_types
        return_types.append(parse_type(cursor))
    raise_error(
        ErrorKind.UNEXPECTED_TOKEN,
        "Unexpected end of input",
        expected="block",
        got="nothing",
    )


def get_op_literal(token: Token) -> Op:
    """Convert a literal token into a push op for its value."""
    value = token.value
    if value == "true":
        return Op(True, OpKind.PUSH_BOOL, token.location)
    if value == "false":
        return Op(False, OpKind.PUSH_BOOL, token.location)
    if value.isdigit() or is_negative_integer_literal(value):
        return Op(int(value), OpKind.PUSH_INT, token.location)
    if value.startswith("'") and value.endswith("'") and len(value) == 3:
        return Op(ord(value[1]), OpKind.PUSH_CHAR, token.location)
    if value.startswith('"') and value.endswith('"'):
        return Op(value[1:-1], OpKind.PUSH_STR, token.location)
    raise ValueError(f"Token `{token.value}` is not a literal")


def _parse_compound_assign(
    cursor: Cursor[Token],
    token: Token,
    op_kind: OpKind,
    function_name: str,
) -> Op:
    """Parse a compound assignment operator (+= or -=)."""
    next_token = expect_token(cursor, kind=TokenKind.IDENTIFIER)
    identifier = token_to_op(next_token, cursor, function_name)
    assert isinstance(identifier, Op), "Expected identifier"
    variable_name = identifier.value
    assert isinstance(variable_name, str), "Expected variable name"
    return Op(variable_name, op_kind, token.location)


def get_op_operator(token: Token, cursor: Cursor[Token], function_name: str) -> Op:
    """Convert an operator token into its op, handling assignments and lambdas."""
    assert len(Operator) == 23, "Exhaustive handling for `Operator`"

    operator = Operator.from_str(token.value)
    assert operator, f"Token `{token.value}` is not an operator"

    match operator:
        case Operator.AND:
            return Op(operator, OpKind.AND, token.location)
        case Operator.BIT_AND:
            return Op(operator, OpKind.BIT_AND, token.location)
        case Operator.BIT_NOT:
            return Op(operator, OpKind.BIT_NOT, token.location)
        case Operator.BIT_OR:
            return Op(operator, OpKind.BIT_OR, token.location)
        case Operator.BIT_XOR:
            return Op(operator, OpKind.BIT_XOR, token.location)
        case Operator.ASSIGN:
            next_token = expect_token(cursor, kind=TokenKind.IDENTIFIER)
            identifier = token_to_op(next_token, cursor, function_name)
            assert isinstance(identifier, Op), "Expected identifier"

            variable_name = identifier.value
            assert isinstance(variable_name, str), "Expected variable name"

            type_annotation = None
            colon = cursor.peek()
            if colon and colon.value == ":":
                cursor.position += 1
                type_annotation = parse_type(cursor)

            return Op(
                variable_name,
                OpKind.ASSIGN_VARIABLE,
                identifier.location,
                type_annotation=type_annotation,
            )
        case Operator.ASSIGN_DECREMENT:
            return _parse_compound_assign(
                cursor, token, OpKind.ASSIGN_DECREMENT, function_name
            )
        case Operator.ASSIGN_INCREMENT:
            return _parse_compound_assign(
                cursor, token, OpKind.ASSIGN_INCREMENT, function_name
            )
        case Operator.DIVISION:
            return Op(operator, OpKind.DIV, token.location)
        case Operator.EQ:
            return Op(operator, OpKind.EQ, token.location)
        case Operator.GE:
            return Op(operator, OpKind.GE, token.location)
        case Operator.GT:
            return Op(operator, OpKind.GT, token.location)
        case Operator.LE:
            return Op(operator, OpKind.LE, token.location)
        case Operator.LT:
            return Op(operator, OpKind.LT, token.location)
        case Operator.MINUS:
            return Op(operator, OpKind.SUB, token.location)
        case Operator.MODULO:
            return Op(operator, OpKind.MOD, token.location)
        case Operator.MULTIPLICATION:
            return Op(operator, OpKind.MUL, token.location)
        case Operator.NE:
            return Op(operator, OpKind.NE, token.location)
        case Operator.NOT:
            return Op(operator, OpKind.NOT, token.location)
        case Operator.OR:
            return Op(operator, OpKind.OR, token.location)
        case Operator.PLUS:
            return Op(operator, OpKind.ADD, token.location)
        case Operator.SHL:
            return Op(operator, OpKind.SHL, token.location)
        case Operator.SHR:
            return Op(operator, OpKind.SHR, token.location)
        case None:
            raise ValueError(f"`{token.value}` is not an operator")


def expect_token(
    cursor: Cursor[Token],
    value: str | None = None,
    kind: TokenKind | None = None,
) -> Token:
    """Consume the next token, raising an error if it does not match."""
    next_token = cursor.pop()
    if not next_token or next_token.kind == TokenKind.EOF:
        expected = f"`{value}`" if value else (f"{kind.name}" if kind else "token")
        last_location = (
            next_token.location
            if next_token
            else (
                cursor.sequence[cursor.position - 1].location
                if cursor.position > 0
                else None
            )
        )
        raise_error(
            ErrorKind.UNEXPECTED_TOKEN,
            "Unexpected end of input",
            last_location,
            expected=expected,
            got="end of input",
        )
    if value and value != next_token.value:
        raise_error(
            ErrorKind.UNEXPECTED_TOKEN,
            "Unexpected token",
            next_token.location,
            expected=f"`{value}`",
            got=f"`{next_token.value}`",
        )
    if kind and kind != next_token.kind:
        raise_error(
            ErrorKind.UNEXPECTED_TOKEN,
            "Unexpected token",
            next_token.location,
            expected=f"`{kind.name}`",
            got=f"`{next_token.kind.name}`",
        )
    return next_token
