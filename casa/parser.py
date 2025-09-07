from typing import assert_never

from casa.common import (
    GLOBAL_IDENTIFIERS,
    Cursor,
    Delimiter,
    Function,
    Intrinsic,
    Keyword,
    Op,
    Operator,
    OpKind,
    Token,
    TokenKind,
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


def token_to_op(token: Token, cursor: Cursor[Token]) -> Op | None:
    assert len(TokenKind) == 7, "Exhaustive handling for `TokenKind`"

    match token.kind:
        case TokenKind.DELIMITER:
            return get_op_delimiter(token, cursor)
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


def get_op_delimiter(token: Token, cursor: Cursor[Token]) -> Op | None:
    assert len(Delimiter) == 5, "Exhaustive handling for `Delimiter`"

    delimiter = Delimiter.from_str(token.value)
    match delimiter:
        case None:
            return None
        case Delimiter.COMMA:
            return None
        case Delimiter.OPEN_BRACE:
            return None
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


def parse_block_ops(cursor: Cursor[Token]) -> list[Op]:
    open_brace = cursor.pop()
    if not open_brace or open_brace.value != "{":
        raise SyntaxError("Expected `{` but got nothing")

    ops: list[Op] = []
    while token := cursor.pop():
        if token.value == "}":
            return ops
        if op := token_to_op(token, cursor):
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
    assert len(Keyword) == 1, "Exhaustive handling for `Keyword"

    keyword = Keyword.from_lowercase(token.value)
    assert keyword, "Expected valid keyword"

    match keyword:
        case Keyword.FN:
            cursor.position -= 1
            function = parse_function(cursor)
            if GLOBAL_IDENTIFIERS.get(function.name):
                raise NameError(f"Identifier `{function.name}` is already defined")
            GLOBAL_IDENTIFIERS[function.name] = function
            return None
        case _:
            assert_never(keyword)


def parse_function(cursor: Cursor[Token]) -> Function:
    fn = cursor.pop()
    if not fn:
        raise SyntaxError("Expected `fn` but got nothing")

    name = cursor.pop()
    if not name:
        raise SyntaxError("Expected function name but got nothing")

    # TODO: Function signature

    open_bracket = cursor.pop()
    if not open_bracket or open_bracket.value != "{":
        raise SyntaxError(f"Expected `{{` but got `{open_bracket.value}`")  # type: ignore

    ops = []
    while token := cursor.pop():
        if token.value == "}":
            return Function(name.value, ops, name.location)
        ops.append(token_to_op(token, cursor))
    raise SyntaxError(f"Expected `}}` but got nothing")


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
