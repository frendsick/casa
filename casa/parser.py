from typing import assert_never

from casa.common import (
    Cursor,
    Delimiter,
    Intrinsic,
    Op,
    Operator,
    OpKind,
    Token,
    TokenKind,
)

INTRINSIC_TO_OPKIND = {
    Intrinsic.LOAD: OpKind.LOAD,
    Intrinsic.PRINT: OpKind.PRINT,
    Intrinsic.STORE: OpKind.STORE,
}


def parse_ops(tokens: list[Token]) -> list[Op]:
    cursor = Cursor(sequence=tokens)
    ops: list[Op] = []
    while token := cursor.pop():
        if op := token_to_op(token, cursor):
            ops.append(op)
    return ops


def token_to_op(token: Token, cursor: Cursor[Token]) -> Op | None:
    assert len(TokenKind) == 5, "Exhaustive handling for `TokenKind`"

    match token.kind:
        case TokenKind.DELIMITER:
            return get_op_delimiter(token, cursor)
        case TokenKind.EOF:
            return None
        case TokenKind.LITERAL:
            return get_op_literal(token)
        case TokenKind.INTRINSIC:
            return get_op_intrinsic(token)
        case TokenKind.OPERATOR:
            return get_op_operator(token)
        case _:
            assert_never(token.kind)


def get_op_delimiter(token: Token, cursor: Cursor[Token]) -> Op | None:
    delimiter = Delimiter.from_str(token.value)
    match delimiter:
        case None:
            return None
        case Delimiter.COMMA:
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


def get_op_literal(token: Token) -> Op:
    if token.value.isdigit():
        return Op(int(token.value), OpKind.PUSH_INT, token.location)
    raise NotImplementedError(token.value)


def get_op_operator(token: Token) -> Op:
    assert len(Operator) == 1, "Exhaustive handling for `Operator`"
    match token.value:
        case "+":
            return Op(Operator.PLUS, OpKind.ADD, token.location)
        case _:
            raise NotImplementedError(token.value)
