from dataclasses import dataclass
from typing import assert_never

from casa.common import Intrinsic, Op, Operator, OpKind, Token, TokenKind


@dataclass
class Parser:
    cursor: int
    tokens: list[Token]

    def parse(self) -> list[Op]:
        ops = []
        while op := self.parse_op():
            ops.append(op)
        return ops

    def is_finished(self) -> bool:
        return self.cursor >= len(self.tokens)

    def peek_op(self) -> Op | None:
        assert len(OpKind) == 3, "Exhaustive handling for `OpKind`"

        if self.is_finished():
            return None

        token = self.tokens[self.cursor]
        match token.kind:
            case TokenKind.EOF:
                return None
            case TokenKind.INTRINSIC:
                return self.get_op_intrinsic(token)
            case TokenKind.LITERAL:
                return self.get_op_literal(token)
            case TokenKind.OPERATOR:
                return self.get_op_operator(token)
            case _:
                assert_never(token.kind)

    def parse_op(self) -> Op | None:
        if op := self.peek_op():
            self.cursor += 1
            return op

    def get_op_literal(self, token: Token) -> Op:
        if token.value.isdigit():
            return Op(int(token.value), OpKind.PUSH_INT, token.location)
        raise NotImplementedError(token.value)

    def get_op_intrinsic(self, token: Token) -> Op:
        assert len(Intrinsic) == 1, "Exhaustive handling for `Intrinsic`"
        match token.value:
            case "print":
                return Op(Intrinsic.PRINT, OpKind.PRINT, token.location)
            case _:
                raise NotImplementedError(token.value)

    def get_op_operator(self, token: Token) -> Op:
        assert len(Operator) == 1, "Exhaustive handling for `Operator`"
        match token.value:
            case "+":
                return Op(Operator.PLUS, OpKind.ADD, token.location)
            case _:
                raise NotImplementedError(token.value)


def parse_ops(tokens: list[Token]) -> list[Op]:
    parser = Parser(cursor=0, tokens=tokens)
    return parser.parse()
