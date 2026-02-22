from pathlib import Path
from typing import assert_never

from casa.common import (
    BUILTIN_TYPES,
    GLOBAL_FUNCTIONS,
    GLOBAL_SCOPE_LABEL,
    GLOBAL_STRUCTS,
    GLOBAL_VARIABLES,
    INCLUDED_FILES,
    Cursor,
    Delimiter,
    Function,
    Intrinsic,
    Keyword,
    Location,
    Member,
    Op,
    Operator,
    OpKind,
    Parameter,
    Signature,
    Span,
    Struct,
    Token,
    TokenKind,
    Type,
    Variable,
)
from casa.error import CasaError, CasaErrorCollection, ErrorKind, raise_error
from casa.lexer import is_negative_integer_literal, lex_file

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
    Intrinsic.SYSCALL0: OpKind.SYSCALL0,
    Intrinsic.SYSCALL1: OpKind.SYSCALL1,
    Intrinsic.SYSCALL2: OpKind.SYSCALL2,
    Intrinsic.SYSCALL3: OpKind.SYSCALL3,
    Intrinsic.SYSCALL4: OpKind.SYSCALL4,
    Intrinsic.SYSCALL5: OpKind.SYSCALL5,
    Intrinsic.SYSCALL6: OpKind.SYSCALL6,
}


def handle_fstring(
    start_token: Token,
    cursor: Cursor[Token],
    function_name: str,
    ops: list[Op],
) -> None:
    part_count = 0
    while token := cursor.pop():
        match token.kind:
            case TokenKind.FSTRING_TEXT:
                ops.append(Op(token.value, OpKind.PUSH_STR, token.location))
                part_count += 1
            case TokenKind.FSTRING_EXPR_START:
                expr_ops: list[Op] = []
                while (t := cursor.pop()) and t.kind != TokenKind.FSTRING_EXPR_END:
                    if t.kind == TokenKind.FSTRING_START:
                        handle_fstring(t, cursor, function_name, expr_ops)
                        continue
                    if op := token_to_op(t, cursor, function_name):
                        expr_ops.append(op)
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


def parse_ops(tokens: list[Token]) -> list[Op]:
    cursor = Cursor(sequence=tokens)
    ops: list[Op] = []
    while token := cursor.pop():
        if token.kind == TokenKind.FSTRING_START:
            handle_fstring(token, cursor, GLOBAL_SCOPE_LABEL, ops)
            continue
        if op := token_to_op(token, cursor):
            ops.append(op)
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


def resolve_identifiers(
    ops: list[Op],
    function: Function | None = None,
) -> list[Op]:
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

                # Gather captures for the lambda function
                # Local variables shadow globals with the same name
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
                    function_name = identifier[1:]
                    if not function_name:
                        errors.append(
                            CasaError(
                                ErrorKind.SYNTAX,
                                "Expected function name after `&`",
                                op.location,
                            )
                        )
                        continue
                    global_function = GLOBAL_FUNCTIONS.get(function_name)
                    if not global_function:
                        errors.append(
                            CasaError(
                                ErrorKind.UNDEFINED_NAME,
                                f"Function `{function_name}` is not defined",
                                op.location,
                            )
                        )
                        continue
                    op.kind = OpKind.FN_PUSH
                    op.value = function_name
                    _mark_used_and_resolve(global_function)
                    continue

                # Check different identifiers
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

                errors.append(
                    CasaError(
                        ErrorKind.UNDEFINED_NAME,
                        f"Identifier `{identifier}` is not defined",
                        op.location,
                    )
                )
            case OpKind.INCLUDE_FILE:
                included_file = op.value
                assert isinstance(included_file, Path), "Expected included file path"

                if included_file not in INCLUDED_FILES:
                    INCLUDED_FILES.add(included_file)

                    # Include the ops from included file
                    included_tokens = lex_file(included_file)
                    included_ops = parse_ops(included_tokens)
                    ops = ops[:index] + included_ops + ops[index:]

    if errors:
        raise CasaErrorCollection(errors)

    return ops


def token_to_op(
    token: Token,
    cursor: Cursor[Token],
    function_name: str = GLOBAL_SCOPE_LABEL,
) -> Op | None:
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
    assert len(Delimiter) == 11, "Exhaustive handling for `Delimiter`"

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
        case _:
            assert_never(delimiter)


def expect_delimiter(cursor: Cursor[Token], expected: Delimiter) -> Delimiter | None:
    token = cursor.peek()
    if not token:
        return None

    delimiter = Delimiter.from_str(token.value)
    if delimiter != expected:
        return None

    cursor.position += 1
    return delimiter


def parse_block_ops(cursor: Cursor[Token], function_name: str) -> list[Op]:
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
        if op := token_to_op(token, cursor, function_name):
            ops.append(op)
    raise_error(ErrorKind.UNMATCHED_BLOCK, "Unclosed block", open_brace.location)


def get_op_array(cursor: Cursor[Token]) -> Op:
    open_bracket = expect_token(cursor, value="[")

    array_items = []
    while not expect_delimiter(cursor, Delimiter.CLOSE_BRACKET):
        next_tok = cursor.peek()
        if next_tok and next_tok.value == "[":
            # Nested array literal
            op = get_op_array(cursor)
        else:
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
            assert item_op is not None, "Array item token always produces an Op"
            op = item_op
        array_items.append(op)

        if expect_delimiter(cursor, Delimiter.COMMA):
            continue

    return Op(array_items, OpKind.PUSH_ARRAY, open_bracket.location)


def get_op_intrinsic(token: Token) -> Op:
    assert len(INTRINSIC_TO_OPKIND) == len(Intrinsic), "Exhaustive handling for `Intrinsic`"  # fmt: skip

    intrinsic = Intrinsic.from_lowercase(token.value)
    assert intrinsic, f"Token `{token.value}` is not an intrinsic"
    return Op(intrinsic, INTRINSIC_TO_OPKIND[intrinsic], token.location)


def get_op_keyword(
    token: Token,
    cursor: Cursor[Token],
    function_name: str,
) -> Op | None:
    assert len(Keyword) == 15, "Exhaustive handling for `Keyword"

    keyword = Keyword.from_lowercase(token.value)
    assert keyword, f"Token `{token.value}` is not a keyword"

    match keyword:
        case Keyword.BREAK:
            return Op(keyword, OpKind.WHILE_BREAK, token.location)
        case Keyword.CONTINUE:
            return Op(keyword, OpKind.WHILE_CONTINUE, token.location)
        case Keyword.DO:
            return Op(keyword, OpKind.WHILE_CONDITION, token.location)
        case Keyword.DONE:
            return Op(keyword, OpKind.WHILE_END, token.location)
        case Keyword.ELIF:
            return Op(keyword, OpKind.IF_ELIF, token.location)
        case Keyword.ELSE:
            return Op(keyword, OpKind.IF_ELSE, token.location)
        case Keyword.FN:
            if function_name != GLOBAL_SCOPE_LABEL:
                raise_error(
                    ErrorKind.INVALID_SCOPE,
                    "Functions should be defined in the global scope",
                    token.location,
                )

            cursor.position -= 1
            function = parse_function(cursor)
            if GLOBAL_FUNCTIONS.get(function.name):
                raise_error(
                    ErrorKind.DUPLICATE_NAME,
                    f"Identifier `{function.name}` is already defined",
                    function.location,
                )

            GLOBAL_FUNCTIONS[function.name] = function
            return None
        case Keyword.FI:
            return Op(keyword, OpKind.IF_END, token.location)
        case Keyword.IF:
            return Op(keyword, OpKind.IF_START, token.location)
        case Keyword.IMPL:
            if function_name != GLOBAL_SCOPE_LABEL:
                raise_error(
                    ErrorKind.INVALID_SCOPE,
                    "Implementation blocks should be defined in the global scope",
                    token.location,
                )

            cursor.position -= 1
            parse_impl_block(cursor)
            return None
        case Keyword.INCLUDE:
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
            return Op(
                included_path.resolve(),
                OpKind.INCLUDE_FILE,
                string_literal.location,
            )
        case Keyword.RETURN:
            return Op(keyword, OpKind.FN_RETURN, token.location)
        case Keyword.STRUCT:
            if function_name != GLOBAL_SCOPE_LABEL:
                raise_error(
                    ErrorKind.INVALID_SCOPE,
                    "Structs should be defined in the global scope",
                    token.location,
                )

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
            return None
        case Keyword.THEN:
            return Op(keyword, OpKind.IF_CONDITION, token.location)
        case Keyword.WHILE:
            return Op(keyword, OpKind.WHILE_START, token.location)
        case _:
            assert_never(keyword)


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

    inner = parse_type(cursor)
    expect_token(cursor, value="]")
    return f"{base.value}[{inner}]"


def get_op_type_cast(cursor: Cursor[Token]) -> Op:
    open_paren = expect_token(cursor, value="(")
    cast_type = parse_type(cursor)
    close_paren = expect_token(cursor, value=")")

    open_offset = open_paren.location.span.offset
    close_offset = close_paren.location.span.offset
    cast_span_length = close_offset - open_offset + 1
    location = Location(open_paren.location.file, Span(open_offset, cast_span_length))
    return Op(cast_type, OpKind.TYPE_CAST, location)


def parse_impl_block(cursor: Cursor[Token]):
    expect_token(cursor, value="impl")
    impl_type = parse_type(cursor)

    expect_token(cursor, value="{")
    while (fn := cursor.peek()) and fn.value == "fn":
        function = parse_function(cursor)
        function.name = f"{impl_type}::{function.name}"
        if GLOBAL_FUNCTIONS.get(function.name):
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Identifier `{function.name}` is already defined",
                function.location,
            )

        GLOBAL_FUNCTIONS[function.name] = function
    expect_token(cursor, value="}")


def parse_struct(cursor: Cursor[Token]) -> Struct:
    expect_token(cursor, value="struct")
    struct_name = expect_token(cursor, kind=TokenKind.IDENTIFIER)

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

        # Getter
        getter_name = f"{struct_name.value}::{member_name.value}"
        if getter_name in GLOBAL_FUNCTIONS:
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Function `{getter_name}` is already defined",
                member_name.location,
            )
        member_location = member_name.location
        getter_ops: list[Op] = []
        getter_ops.append(Op("ptr", OpKind.TYPE_CAST, member_location))
        getter_ops.append(Op(len(members) * 8, OpKind.PUSH_INT, member_location))
        getter_ops.append(Op(Operator.PLUS, OpKind.ADD, member_location))
        getter_ops.append(Op(Intrinsic.LOAD64, OpKind.LOAD64, member_location))
        getter_ops.append(Op(member_type_str, OpKind.TYPE_CAST, member_location))
        getter_params = [Parameter(struct_name.value)]
        getter_signature = Signature(getter_params, [member_type_str])
        getter = Function(getter_name, getter_ops, member_location, getter_signature)
        GLOBAL_FUNCTIONS[getter_name] = getter

        # Setter
        setter_name = f"{struct_name.value}::set_{member_name.value}"
        if setter_name in GLOBAL_FUNCTIONS:
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Function `{setter_name}` is already defined",
                member_name.location,
            )
        setter_ops: list[Op] = []
        setter_ops.append(Op("ptr", OpKind.TYPE_CAST, member_location))
        setter_ops.append(Op(len(members) * 8, OpKind.PUSH_INT, member_location))
        setter_ops.append(Op(Operator.PLUS, OpKind.ADD, member_location))
        setter_ops.append(Op(Intrinsic.STORE64, OpKind.STORE64, member_location))
        setter_params = [Parameter(struct_name.value), Parameter(member_type_str)]
        setter_signature = Signature(setter_params, [])
        setter = Function(setter_name, setter_ops, member_location, setter_signature)
        GLOBAL_FUNCTIONS[setter_name] = setter

        # Add to members list
        members.append(Member(member_name.value, member_type_str))

    return Struct(struct_name.value, members, struct_name.location)


def parse_type_vars(cursor: Cursor[Token]) -> set[str]:
    expect_token(cursor, value="[")
    type_vars: set[str] = set()
    while token := cursor.pop():
        if token.value == "]":
            if not type_vars:
                raise_error(
                    ErrorKind.SYNTAX,
                    "Empty type parameter list `[]` is not allowed",
                    token.location,
                )
            return type_vars
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
            type_vars.add(token.value)
        elif token.value != ",":
            raise_error(
                ErrorKind.UNEXPECTED_TOKEN,
                "Unexpected token in type parameters",
                token.location,
                expected="type variable name",
                got=f"`{token.value}`",
            )
    raise_error(ErrorKind.SYNTAX, "Expected `]` to close type parameters")


def parse_function(cursor: Cursor[Token]) -> Function:
    expect_token(cursor, value="fn")
    name = expect_token(cursor, kind=TokenKind.IDENTIFIER)

    # Parse optional type parameters: fn name[T1 T2] ...
    type_vars: set[str] = set()
    next_token = cursor.peek()
    if next_token and next_token.value == "[":
        type_vars = parse_type_vars(cursor)

    signature = parse_signature(cursor)
    signature.type_vars = type_vars

    ops: list[Op] = []
    variables: list[Variable] = []
    for param in signature.parameters:
        # Named parameters are popped from the stack into local variables via
        # implicit `ASSIGN_VARIABLE` ops prepended to the function body.
        # Unnamed parameters stay on the stack as passthrough values.
        if param.name:
            variables.append(Variable(param.name, param.typ))
            ops.append(Op(param.name, OpKind.ASSIGN_VARIABLE, name.location))

    ops += parse_block_ops(cursor, name.value)
    function = Function(
        name.value, ops, name.location, signature=signature, variables=variables
    )
    return function


def parse_signature(cursor: Cursor[Token]) -> Signature:
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
    return_types: list[Type] = []
    while cursor.peek():
        if cursor.peek().value == "{":  # type: ignore[union-attr]
            return return_types
        return_types.append(parse_type(cursor))
    raise_error(
        ErrorKind.UNEXPECTED_TOKEN,
        "Unexpected end of input",
        expected="block",
        got="nothing",
    )


def get_op_literal(token: Token) -> Op:
    value = token.value
    if value == "true":
        return Op(True, OpKind.PUSH_BOOL, token.location)
    if value == "false":
        return Op(False, OpKind.PUSH_BOOL, token.location)
    if value == "none":
        return Op(None, OpKind.PUSH_NONE, token.location)
    if value == "some":
        return Op(None, OpKind.SOME, token.location)
    if value.isdigit() or is_negative_integer_literal(value):
        return Op(int(value), OpKind.PUSH_INT, token.location)
    if value.startswith('"') and value.endswith('"'):
        return Op(value[1:-1], OpKind.PUSH_STR, token.location)
    raise ValueError(f"Token `{token.value}` is not a literal")


def get_op_operator(token: Token, cursor: Cursor[Token], function_name: str) -> Op:
    assert len(Operator) == 19, "Exhaustive handling for `Operator`"

    operator = Operator.from_str(token.value)
    assert operator, f"Token `{token.value}` is not an operator"

    match operator:
        case Operator.AND:
            return Op(operator, OpKind.AND, token.location)
        case Operator.ASSIGN:
            next_token = expect_token(cursor, kind=TokenKind.IDENTIFIER)
            identifier = token_to_op(next_token, cursor, function_name)
            assert identifier, "Expected identifier"

            variable_name = identifier.value
            assert isinstance(variable_name, str), "Expected variable name"

            return Op(variable_name, OpKind.ASSIGN_VARIABLE, identifier.location)
        case Operator.ASSIGN_DECREMENT:
            next_token = expect_token(cursor, kind=TokenKind.IDENTIFIER)
            identifier = token_to_op(next_token, cursor, function_name)
            assert identifier, "Expected identifier"

            variable_name = identifier.value
            assert isinstance(variable_name, str), "Expected variable name"

            return Op(variable_name, OpKind.ASSIGN_DECREMENT, token.location)
        case Operator.ASSIGN_INCREMENT:
            next_token = expect_token(cursor, kind=TokenKind.IDENTIFIER)
            identifier = token_to_op(next_token, cursor, function_name)
            assert identifier, "Expected identifier"

            variable_name = identifier.value
            assert isinstance(variable_name, str), "Expected variable name"

            return Op(variable_name, OpKind.ASSIGN_INCREMENT, identifier.location)
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
