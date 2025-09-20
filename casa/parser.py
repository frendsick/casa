from typing import assert_never

from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_SCOPE_LABEL,
    GLOBAL_STRUCTS,
    GLOBAL_VARIABLES,
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
):
    for op in ops:
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
                    raise NameError(f"Lambda function `{function_name}` is not defined")

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

                resolve_identifiers(lambda_function.ops, lambda_function)
            case OpKind.IDENTIFIER:
                identifier = op.value
                assert isinstance(identifier, str), "Expected identifier name"

                # Check different identifiers
                if global_function := GLOBAL_FUNCTIONS.get(identifier):
                    op.kind = OpKind.FN_CALL
                    if not global_function.is_used:
                        global_function.is_used = True
                        resolve_identifiers(global_function.ops, global_function)
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

                raise NameError(f"Identifier `{identifier}` is not defined")


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
        raise SyntaxError(f"Expected `{{` but got `{open_brace.value}`")  # type: ignore

    ops: list[Op] = []
    while token := cursor.pop():
        if token.value == "}":
            return ops
        if op := token_to_op(token, cursor, function_name):
            ops.append(op)
    raise SyntaxError("Unclosed block")


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
    assert len(Keyword) == 14, "Exhaustive handling for `Keyword"

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
                raise SyntaxError("Functions should be defined in the global scope")

            cursor.position -= 1
            function = parse_function(cursor)
            if GLOBAL_FUNCTIONS.get(function.name):
                raise NameError(f"Identifier `{function.name}` is already defined")

            GLOBAL_FUNCTIONS[function.name] = function
            return None
        case Keyword.IF:
            return Op(keyword, OpKind.IF_START, token.location)
        case Keyword.IMPL:
            if function_name != GLOBAL_SCOPE_LABEL:
                raise SyntaxError(
                    "Implementation blocks should be defined in the global scope"
                )

            cursor.position -= 1
            parse_impl_block(cursor)
            return None
        case Keyword.FI:
            return Op(keyword, OpKind.IF_END, token.location)
        case Keyword.RETURN:
            return Op(keyword, OpKind.FN_RETURN, token.location)
        case Keyword.STRUCT:
            if function_name != GLOBAL_SCOPE_LABEL:
                raise SyntaxError("Structs should be defined in the global scope")

            cursor.position -= 1

            struct = parse_struct(cursor)
            GLOBAL_STRUCTS[struct.name] = struct

            for member in struct.members:
                if member.name in GLOBAL_FUNCTIONS:
                    raise NameError(f"Function `{member.name}` already exists")
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
            raise NameError(f"Identifier `{function.name}` is already defined")

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
            raise SyntaxError(f"Expected identifier but got `{member_name.kind}`")

        expect_token(cursor, value=":")

        member_type = expect_token(cursor, kind=TokenKind.IDENTIFIER)

        # Getter
        getter_name = f"{struct_name.value}::{member_name.value}"
        if getter_name in GLOBAL_FUNCTIONS:
            raise NameError(f"Function `{getter_name}` is already defined")
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
            raise NameError(f"Function `{setter_name}` is already defined")
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


def parse_function(cursor: Cursor[Token]) -> Function:
    expect_token(cursor, value="fn")
    name = expect_token(cursor, kind=TokenKind.IDENTIFIER)
    signature = parse_signature(cursor)

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
        raise SyntaxError("Expected `->` or `{` but got nothing")

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
            raise SyntaxError(f"Expected identifier but got `{name_or_type.kind}`")

        # Parse typed parameter
        next_token = cursor.peek()
        if next_token and next_token.value == ":":
            cursor.position += 1
            typ = expect_token(cursor, kind=TokenKind.IDENTIFIER)
            parameters.append(Parameter(typ.value, name_or_type.value))
        else:
            parameters.append(Parameter(name_or_type.value))
    raise SyntaxError("Expected `->` or block but got nothing")


def parse_return_types(cursor: Cursor[Token]) -> list[Type]:
    return_types: list[Type] = []
    while return_type := cursor.pop():
        if return_type.value == "{":
            cursor.position -= 1
            return return_types
        return_types.append(return_type.value)
    raise SyntaxError("Expected block but got nothing")


def get_op_literal(token: Token) -> Op:
    value = token.value
    if value == "true":
        return Op(True, OpKind.PUSH_BOOL, token.location)
    if value == "false":
        return Op(False, OpKind.PUSH_BOOL, token.location)
    if value.isdigit():
        return Op(int(value), OpKind.PUSH_INT, token.location)
    if value.startswith('"') and value.endswith('"'):
        return Op(value[1:-1], OpKind.PUSH_STR, token.location)
    raise ValueError(f"Token `{token.value}` is not a literal")


def get_op_operator(token: Token, cursor: Cursor[Token], function_name: str) -> Op:
    assert len(Operator) == 17, "Exhaustive handling for `Operator`"

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
        case None:
            raise ValueError(f"`{token.value}` is not an operator")


def expect_token(
    cursor: Cursor[Token],
    value: str | None = None,
    kind: TokenKind | None = None,
) -> Token:
    next_token = cursor.pop()
    if not next_token:
        raise SyntaxError(f"Expected `{value}` but got nothing")
    if value and value != next_token.value:
        raise SyntaxError(f"Expected `{value}` but got `{next_token.value}`")
    if kind and kind != next_token.kind:
        raise SyntaxError(f"Expected `{kind}` but got `{next_token.kind}`")
    return next_token
