from typing import assert_never

from casa.common import (
    GLOBAL_IDENTIFIERS,
    GLOBAL_SCOPE_LABEL,
    Cursor,
    Delimiter,
    Function,
    Intrinsic,
    Keyword,
    Op,
    Operator,
    OpKind,
    Signature,
    Token,
    TokenKind,
    Type,
)

INTRINSIC_TO_OPKIND = {
    Intrinsic.DROP: OpKind.DROP,
    Intrinsic.DUP: OpKind.DUP,
    Intrinsic.OVER: OpKind.OVER,
    Intrinsic.ROT: OpKind.ROT,
    Intrinsic.SWAP: OpKind.SWAP,
    Intrinsic.LOAD: OpKind.LOAD,
    Intrinsic.STORE: OpKind.STORE,
    Intrinsic.PRINT: OpKind.PRINT,
    Intrinsic.EXEC: OpKind.EXEC_FN,
}


def parse_ops(tokens: list[Token]) -> list[Op]:
    cursor = Cursor(sequence=tokens)
    ops: list[Op] = []
    while token := cursor.pop():
        if op := token_to_op(token, cursor):
            ops.append(op)
    return ops


def resolve_identifiers(ops: list[Op]):
    for op in ops:
        if op.kind != OpKind.IDENTIFIER:
            continue

        assert isinstance(op.value, str), "Expected identifier name"
        identifier_name = op.value
        identifier = GLOBAL_IDENTIFIERS.get(identifier_name)

        match identifier:
            case Function() as f:
                op.kind = OpKind.CALL_FN
                if not f.is_used:
                    f.is_used = True
                    resolve_identifiers(f.ops)
            case None:
                raise NameError(f"Identifier `{identifier_name}` is not defined")


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
            return get_op_keyword(token, cursor)
        case TokenKind.OPERATOR:
            return get_op_operator(token)
        case _:
            assert_never(token.kind)


def get_op_delimiter(token: Token, cursor: Cursor[Token], function_name) -> Op | None:
    assert len(Delimiter) == 5, "Exhaustive handling for `Delimiter`"

    delimiter = Delimiter.from_str(token.value)
    match delimiter:
        case None:
            return None
        case Delimiter.COMMA:
            return None
        # Lambda function
        case Delimiter.OPEN_BRACE:
            cursor.position -= 1
            ops = parse_block_ops(cursor, function_name)
            lambda_name = f"lambda__{function_name}_o{token.location.span.offset}"
            lambda_function = Function(lambda_name, ops, token.location)
            GLOBAL_IDENTIFIERS[lambda_name] = lambda_function
            return Op(lambda_name, OpKind.PUSH_FN, token.location)
        case Delimiter.CLOSE_BRACE:
            return None
        case Delimiter.OPEN_BRACKET:
            cursor.position -= 1
            return get_op_list(cursor)
        case Delimiter.CLOSE_BRACKET:
            return None
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
        raise SyntaxError("Expected `{` but got nothing")

    ops: list[Op] = []
    while token := cursor.pop():
        if token.value == "}":
            return ops
        if op := token_to_op(token, cursor, function_name):
            ops.append(op)
    raise SyntaxError("Unclosed block")


def get_op_list(cursor: Cursor[Token]) -> Op:
    open_bracket = cursor.pop()
    if not open_bracket or open_bracket.value != "[":
        raise SyntaxError("Expected `[` but got nothing")

    # Empty list
    list_items = []
    while not expect_delimiter(cursor, Delimiter.CLOSE_BRACKET):
        value_token = cursor.pop()
        if not value_token:
            raise SyntaxError("Expected list value but got nothing")

        # TODO: Support identifiers
        if value_token.kind != TokenKind.LITERAL:
            raise SyntaxError(f"Expected list value but got `{value_token.value}`")

        op = token_to_op(value_token, cursor)
        list_items.append(op)

        if expect_delimiter(cursor, Delimiter.COMMA):
            continue

    return Op(list_items, OpKind.PUSH_LIST, open_bracket.location)


def get_op_intrinsic(token: Token) -> Op:
    assert len(INTRINSIC_TO_OPKIND) == len(Intrinsic), "Exhaustive handling for `Intrinsic`"  # fmt: skip

    intrinsic = Intrinsic.from_lowercase(token.value)
    assert intrinsic, f"Token `{token.value}` is not an intrinsic"
    return Op(intrinsic, INTRINSIC_TO_OPKIND[intrinsic], token.location)


def get_op_keyword(token: Token, cursor: Cursor[Token]) -> Op | None:
    assert len(Keyword) == 4, "Exhaustive handling for `Keyword"

    keyword = Keyword.from_lowercase(token.value)
    assert keyword, f"Token `{token.value}` is not a keyword"

    match keyword:
        case Keyword.DO:
            return Op(keyword, OpKind.WHILE_CONDITION, token.location)
        case Keyword.DONE:
            return Op(keyword, OpKind.WHILE_END, token.location)
        case Keyword.FN:
            cursor.position -= 1
            function = parse_function(cursor)
            if GLOBAL_IDENTIFIERS.get(function.name):
                raise NameError(f"Identifier `{function.name}` is already defined")
            GLOBAL_IDENTIFIERS[function.name] = function
            return None
        case Keyword.WHILE:
            return Op(keyword, OpKind.WHILE_START, token.location)
        case _:
            assert_never(keyword)


def parse_function(cursor: Cursor[Token]) -> Function:
    fn = cursor.pop()
    if not fn:
        raise SyntaxError("Expected `fn` but got nothing")

    name = cursor.pop()
    if not name:
        raise SyntaxError("Expected function name but got nothing")

    signature = parse_signature(cursor)

    ops = parse_block_ops(cursor, name.value)
    return Function(name.value, ops, name.location, signature)


def parse_signature(cursor: Cursor[Token]) -> Signature:
    parameters = parse_parameters(cursor)
    return_types = []

    next_token = cursor.pop()
    if not next_token:
        raise SyntaxError("Expected `->` or `{` but got nothing")

    if next_token.value == "->":
        return_types = parse_return_types(cursor)

    return Signature(parameters, return_types)


def parse_parameters(cursor: Cursor[Token]) -> list[Type]:
    parameters = []
    while parameter := cursor.pop():
        if parameter.value in ("->", "{"):
            cursor.position -= 1
            return parameters
        parameters.append(parameter.value)
    raise SyntaxError("Expected `->` or block but got nothing")


def parse_return_types(cursor: Cursor[Token]) -> list[Type]:
    return_types = []
    while return_type := cursor.pop():
        if return_type.value == "{":
            cursor.position -= 1
            return return_types
        return_types.append(return_type.value)
    raise SyntaxError("Expected block but got nothing")


def get_op_literal(token: Token) -> Op:
    if token.value.isdigit():
        return Op(int(token.value), OpKind.PUSH_INT, token.location)
    raise ValueError(f"Token `{token.value}` is not a literal")


def get_op_operator(token: Token) -> Op:
    assert len(Operator) == 7, "Exhaustive handling for `Operator`"

    operator = Operator.from_str(token.value)
    assert operator, f"Token `{token.value}` is not an operator"

    match operator:
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
        case Operator.NE:
            return Op(operator, OpKind.NE, token.location)
        case Operator.PLUS:
            return Op(operator, OpKind.ADD, token.location)
        case None:
            raise ValueError(f"`{token.value}` is not an operator")
