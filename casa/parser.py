from typing import assert_never

from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_SCOPE_LABEL,
    GLOBAL_VARIABLES,
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
    Variable,
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
    Intrinsic.EXEC: OpKind.FN_EXEC,
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
            return get_op_keyword(token, cursor)
        case TokenKind.OPERATOR:
            return get_op_operator(token, cursor, function_name)
        case _:
            assert_never(token.kind)


def get_op_delimiter(
    token: Token,
    cursor: Cursor[Token],
    function_name: str,
) -> Op | None:
    assert len(Delimiter) == 6, "Exhaustive handling for `Delimiter`"

    delimiter = Delimiter.from_str(token.value)
    match delimiter:
        case None:
            return None
        case Delimiter.COLON:
            return None
        case Delimiter.COMMA:
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
        raise SyntaxError(f"Expected `{{` but got `{open_brace.value}`")  # type: ignore

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
    assert len(Keyword) == 12, "Exhaustive handling for `Keyword"

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
            cursor.position -= 1
            function = parse_function(cursor)
            if GLOBAL_FUNCTIONS.get(function.name):
                raise NameError(f"Identifier `{function.name}` is already defined")

            GLOBAL_FUNCTIONS[function.name] = function
            return None
        case Keyword.IF:
            return Op(keyword, OpKind.IF_START, token.location)
        case Keyword.FI:
            return Op(keyword, OpKind.IF_END, token.location)
        case Keyword.RETURN:
            return Op(keyword, OpKind.FN_RETURN, token.location)
        case Keyword.THEN:
            return Op(keyword, OpKind.IF_CONDITION, token.location)
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

    ops = parse_block_ops(cursor, name.value)
    function = Function(name.value, ops, name.location)
    return function


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
            next_token = cursor.pop()
            if not next_token:
                raise SyntaxError("Expected variable name but got nothing")

            identifier = token_to_op(next_token, cursor, function_name)
            if not identifier or identifier.kind != OpKind.IDENTIFIER:
                raise SyntaxError(f"Expected identifier but got `{next_token.kind}`")  # type: ignore
            variable_name = identifier.value
            assert isinstance(variable_name, str), "Expected variable name"

            return Op(variable_name, OpKind.ASSIGN_VARIABLE, identifier.location)
        case Operator.ASSIGN_DECREMENT:
            next_token = cursor.pop()
            if not next_token:
                raise SyntaxError("Expected variable name but got nothing")

            identifier = token_to_op(next_token, cursor, function_name)
            if not identifier or identifier.kind != OpKind.IDENTIFIER:
                raise SyntaxError(f"Expected identifier but got `{next_token.kind}`")  # type: ignore
            variable_name = identifier.value
            assert isinstance(variable_name, str), "Expected variable name"

            return Op(variable_name, OpKind.ASSIGN_DECREMENT, token.location)
        case Operator.ASSIGN_INCREMENT:
            next_token = cursor.pop()
            if not next_token:
                raise SyntaxError("Expected variable name but got nothing")

            identifier = token_to_op(next_token, cursor, function_name)
            if not identifier or identifier.kind != OpKind.IDENTIFIER:
                raise SyntaxError(f"Expected identifier but got `{next_token.kind}`")  # type: ignore
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
