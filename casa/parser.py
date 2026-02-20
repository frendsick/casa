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
from casa.error import CasaError, CasaErrorCollection, ErrorKind
from casa.lexer import is_negative_integer_literal, lex_file

INTRINSIC_TO_OPKIND = {
    Intrinsic.ALLOC: OpKind.HEAP_ALLOC,
    Intrinsic.DROP: OpKind.DROP,
    Intrinsic.DUP: OpKind.DUP,
    Intrinsic.EXEC: OpKind.FN_EXEC,
    Intrinsic.LOAD: OpKind.LOAD,
    Intrinsic.OVER: OpKind.OVER,
    Intrinsic.PRINT: OpKind.PRINT,
    Intrinsic.ROT: OpKind.ROT,
    Intrinsic.STORE: OpKind.STORE,
    Intrinsic.SWAP: OpKind.SWAP,
}


def parse_ops(tokens: list[Token]) -> list[Op]:
    cursor = Cursor(sequence=tokens)
    ops: list[Op] = []
    while token := cursor.pop():
        if op := token_to_op(token, cursor):
            ops.append(op)
    return ops


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

                # Function scope
                if GLOBAL_VARIABLES.get(variable_name):
                    continue
                if variable not in function.variables:
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
                for op in lambda_function.ops:
                    if op.kind != OpKind.IDENTIFIER:
                        continue
                    for global_variable in GLOBAL_VARIABLES.values():
                        if global_variable.name == op.value:
                            lambda_function.captures.append(global_variable)
                    if function:
                        for local_variable in function.variables:
                            if op.value == local_variable.name:
                                lambda_function.captures.append(local_variable)

                lambda_function.ops = resolve_identifiers(
                    lambda_function.ops, lambda_function
                )
            case OpKind.IDENTIFIER:
                identifier = op.value
                assert isinstance(identifier, str), "Expected identifier name"

                # Check different identifiers
                if global_function := GLOBAL_FUNCTIONS.get(identifier):
                    op.kind = OpKind.FN_CALL
                    if not global_function.is_used:
                        global_function.is_used = True
                        global_function.ops = resolve_identifiers(
                            global_function.ops, global_function
                        )
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
                if function and identifier in function.variables:
                    op.kind = OpKind.PUSH_VARIABLE
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
    assert len(TokenKind) == 7, "Exhaustive handling for `TokenKind`"

    match token.kind:
        case TokenKind.DELIMITER:
            return get_op_delimiter(token, cursor, function_name)
        case TokenKind.EOF:
            return None
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
        raise CasaErrorCollection(
            CasaError(
                ErrorKind.UNEXPECTED_TOKEN,
                f"Expected `{{` but got `{open_brace.value if open_brace else 'nothing'}`",
                loc,
            )
        )

    ops: list[Op] = []
    while token := cursor.pop():
        if token.value == "}":
            return ops
        if op := token_to_op(token, cursor, function_name):
            ops.append(op)
    raise CasaErrorCollection(
        CasaError(ErrorKind.UNMATCHED_BLOCK, "Unclosed block", open_brace.location)
    )


def get_op_array(cursor: Cursor[Token]) -> Op:
    open_bracket = expect_token(cursor, value="[")

    # Empty list
    array_items = []
    while not expect_delimiter(cursor, Delimiter.CLOSE_BRACKET):
        # TODO: Support identifiers
        value_token = expect_token(cursor, kind=TokenKind.LITERAL)

        op = token_to_op(value_token, cursor)
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
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.INVALID_SCOPE,
                        "Functions should be defined in the global scope",
                        token.location,
                    )
                )

            cursor.position -= 1
            function = parse_function(cursor)
            if GLOBAL_FUNCTIONS.get(function.name):
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.DUPLICATE_NAME,
                        f"Identifier `{function.name}` is already defined",
                        function.location,
                    )
                )

            GLOBAL_FUNCTIONS[function.name] = function
            return None
        case Keyword.FI:
            return Op(keyword, OpKind.IF_END, token.location)
        case Keyword.IF:
            return Op(keyword, OpKind.IF_START, token.location)
        case Keyword.IMPL:
            if function_name != GLOBAL_SCOPE_LABEL:
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.INVALID_SCOPE,
                        "Implementation blocks should be defined in the global scope",
                        token.location,
                    )
                )

            cursor.position -= 1
            parse_impl_block(cursor)
            return None
        case Keyword.INCLUDE:
            string_literal = expect_token(cursor, kind=TokenKind.LITERAL)
            literal_op = get_op_literal(string_literal)
            if literal_op.kind != OpKind.PUSH_STR:
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.UNEXPECTED_TOKEN,
                        f"Expected included file as string literal but got `{string_literal.value}`",
                        string_literal.location,
                    )
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
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.INVALID_SCOPE,
                        "Structs should be defined in the global scope",
                        token.location,
                    )
                )

            cursor.position -= 1

            struct = parse_struct(cursor)
            GLOBAL_STRUCTS[struct.name] = struct

            for member in struct.members:
                if member.name in GLOBAL_FUNCTIONS:
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.DUPLICATE_NAME,
                            f"Function `{member.name}` already exists",
                            struct.location,
                        )
                    )
            return None
        case Keyword.THEN:
            return Op(keyword, OpKind.IF_CONDITION, token.location)
        case Keyword.WHILE:
            return Op(keyword, OpKind.WHILE_START, token.location)
        case _:
            assert_never(keyword)


def get_op_type_cast(cursor: Cursor[Token]) -> Op:
    open_paren = expect_token(cursor, value="(")
    cast_type = expect_token(cursor, kind=TokenKind.IDENTIFIER)
    close_paren = expect_token(cursor, value=")")

    open_offset = open_paren.location.span.offset
    close_offset = close_paren.location.span.offset
    cast_span_length = close_offset - open_offset + 1
    location = Location(open_paren.location.file, Span(open_offset, cast_span_length))
    return Op(cast_type.value, OpKind.TYPE_CAST, location)


def parse_impl_block(cursor: Cursor[Token]):
    expect_token(cursor, value="impl")
    impl_type = expect_token(cursor, kind=TokenKind.IDENTIFIER)

    expect_token(cursor, value="{")
    while (fn := cursor.peek()) and fn.value == "fn":
        function = parse_function(cursor)
        function.name = f"{impl_type.value}::{function.name}"
        if GLOBAL_FUNCTIONS.get(function.name):
            raise CasaErrorCollection(
                CasaError(
                    ErrorKind.DUPLICATE_NAME,
                    f"Identifier `{function.name}` is already defined",
                    function.location,
                )
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
            raise CasaErrorCollection(
                CasaError(
                    ErrorKind.UNEXPECTED_TOKEN,
                    f"Expected identifier but got `{member_name.kind.name}`",
                    member_name.location,
                )
            )

        expect_token(cursor, value=":")

        member_type = expect_token(cursor, kind=TokenKind.IDENTIFIER)

        # Getter
        getter_name = f"{struct_name.value}::{member_name.value}"
        if getter_name in GLOBAL_FUNCTIONS:
            raise CasaErrorCollection(
                CasaError(
                    ErrorKind.DUPLICATE_NAME,
                    f"Function `{getter_name}` is already defined",
                    member_name.location,
                )
            )
        member_location = member_name.location
        getter_ops: list[Op] = []
        getter_ops.append(Op(len(members), OpKind.PUSH_INT, member_location))
        getter_ops.append(Op(Operator.PLUS, OpKind.ADD, member_location))
        getter_ops.append(Op(Intrinsic.LOAD, OpKind.LOAD, member_location))
        getter_params = [Parameter(struct_name.value)]
        getter_signature = Signature(getter_params, [member_type.value])
        getter = Function(getter_name, getter_ops, member_location, getter_signature)
        GLOBAL_FUNCTIONS[getter_name] = getter

        # Setter
        setter_name = f"{struct_name.value}::set_{member_name.value}"
        if setter_name in GLOBAL_FUNCTIONS:
            raise CasaErrorCollection(
                CasaError(
                    ErrorKind.DUPLICATE_NAME,
                    f"Function `{setter_name}` is already defined",
                    member_name.location,
                )
            )
        setter_ops: list[Op] = []
        setter_ops.append(Op(len(members), OpKind.PUSH_INT, member_location))
        setter_ops.append(Op(Operator.PLUS, OpKind.ADD, member_location))
        setter_ops.append(Op(Intrinsic.STORE, OpKind.STORE, member_location))
        setter_params = [Parameter(struct_name.value), Parameter(member_type.value)]
        setter_signature = Signature(setter_params, [])
        setter = Function(setter_name, setter_ops, member_location, setter_signature)
        GLOBAL_FUNCTIONS[setter_name] = setter

        # Add to members list
        members.append(Member(member_name.value, member_type.value))

    return Struct(struct_name.value, members, struct_name.location)


def parse_type_vars(cursor: Cursor[Token]) -> set[str]:
    expect_token(cursor, value="[")
    type_vars: set[str] = set()
    while token := cursor.pop():
        if token.value == "]":
            if not type_vars:
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.SYNTAX,
                        "Empty type parameter list `[]` is not allowed",
                        token.location,
                    )
                )
            return type_vars
        if token.kind == TokenKind.IDENTIFIER:
            if token.value in BUILTIN_TYPES:
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.DUPLICATE_NAME,
                        f"Type variable `{token.value}` shadows built-in type `{token.value}`",
                        token.location,
                    )
                )
            if token.value in GLOBAL_STRUCTS:
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.DUPLICATE_NAME,
                        f"Type variable `{token.value}` shadows struct type `{token.value}`",
                        token.location,
                    )
                )
            type_vars.add(token.value)
        elif token.value != ",":
            raise CasaErrorCollection(
                CasaError(
                    ErrorKind.UNEXPECTED_TOKEN,
                    f"Expected type variable name but got `{token.value}`",
                    token.location,
                )
            )
    raise CasaErrorCollection(
        CasaError(ErrorKind.SYNTAX, "Expected `]` to close type parameters")
    )


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
        raise CasaErrorCollection(
            CasaError(
                ErrorKind.UNEXPECTED_TOKEN,
                "Expected `->` or `{` but got nothing",
            )
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
        if name_or_type.kind != TokenKind.IDENTIFIER:
            raise CasaErrorCollection(
                CasaError(
                    ErrorKind.UNEXPECTED_TOKEN,
                    f"Expected identifier but got `{name_or_type.kind.name}`",
                    name_or_type.location,
                )
            )

        # Parse typed parameter
        next_token = cursor.peek()
        if next_token and next_token.value == ":":
            cursor.position += 1
            typ = expect_token(cursor, kind=TokenKind.IDENTIFIER)
            parameters.append(Parameter(typ.value, name_or_type.value))
        else:
            parameters.append(Parameter(name_or_type.value))
    raise CasaErrorCollection(
        CasaError(ErrorKind.UNEXPECTED_TOKEN, "Expected `->` or block but got nothing")
    )


def parse_return_types(cursor: Cursor[Token]) -> list[Type]:
    return_types: list[Type] = []
    while return_type := cursor.pop():
        if return_type.value == "{":
            cursor.position -= 1
            return return_types
        return_types.append(return_type.value)
    raise CasaErrorCollection(
        CasaError(ErrorKind.UNEXPECTED_TOKEN, "Expected block but got nothing")
    )


def get_op_literal(token: Token) -> Op:
    value = token.value
    if value == "true":
        return Op(True, OpKind.PUSH_BOOL, token.location)
    if value == "false":
        return Op(False, OpKind.PUSH_BOOL, token.location)
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
        raise CasaErrorCollection(
            CasaError(
                ErrorKind.UNEXPECTED_TOKEN,
                f"Expected {expected} but got end of input",
                last_location,
            )
        )
    if value and value != next_token.value:
        raise CasaErrorCollection(
            CasaError(
                ErrorKind.UNEXPECTED_TOKEN,
                f"Expected `{value}` but got `{next_token.value}`",
                next_token.location,
            )
        )
    if kind and kind != next_token.kind:
        raise CasaErrorCollection(
            CasaError(
                ErrorKind.UNEXPECTED_TOKEN,
                f"Expected `{kind.name}` but got `{next_token.kind.name}`",
                next_token.location,
            )
        )
    return next_token
