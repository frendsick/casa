from typing import assert_never

from casa.common import Cursor, Intrinsic, Op, Operator, OpKind, Token, TokenKind

INTRINSIC_TO_OPKIND = {
    Intrinsic.PRINT: OpKind.PRINT,
}


def parse_ops(tokens: list[Token]) -> list[Op]:
    cursor = Cursor(sequence=tokens)
    ops: list[Op] = []
    while token := cursor.pop():
        if token.kind == TokenKind.EOF:
            continue
        ops.append(token_to_op(token))
    return ops


def token_to_op(token: Token) -> Op:
    match token.kind:
        case TokenKind.EOF:
            raise ValueError(f"`{token.kind}` cannot be represented as Op")
        case TokenKind.LITERAL:
            return get_op_literal(token)
        case TokenKind.INTRINSIC:
            intrinsic = Intrinsic.from_lowercase(token.value)
            if not intrinsic:
                raise ValueError(f"Token `{token.value}` is not an intrinsic")
            return Op(intrinsic, INTRINSIC_TO_OPKIND[intrinsic], token.location)
        case TokenKind.OPERATOR:
            return get_op_operator(token)
        case _:
            assert_never(token.kind)


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
