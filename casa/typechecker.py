from typing import assert_never

from casa.common import GLOBAL_IDENTIFIERS, Function, Op, OpKind

type Type = str


# TODO: Support list literals
LITERAL_OP_KINDS = [OpKind.PUSH_INT]


def type_check_ops(ops: list[Op], stack: list[Type] | None = None):
    assert len(OpKind) == 13, "Exhaustive handling for `OpKind`"

    if not stack:
        stack = []
    for op in ops:
        match op.kind:
            case OpKind.CALL_FN:
                pass
            case OpKind.ADD:
                expect_type(stack, "int")
                add_type = stack_pop(stack)
                stack_push(stack, add_type)
            case OpKind.DROP:
                stack_pop(stack)
            case OpKind.DUP:
                a = stack_pop(stack)
                stack_push(stack, a)
                stack_push(stack, a)
            case OpKind.IDENTIFIER:
                raise AssertionError(
                    f"Identifier `{op.value}` should be resolved by the parser"
                )
            case OpKind.LOAD:
                expect_type(stack, "ptr")
                stack_push(stack, "any")
            case OpKind.OVER:
                a = stack_pop(stack)
                b = stack_pop(stack)
                stack_push(stack, b)
                stack_push(stack, a)
                stack_push(stack, b)
            case OpKind.PUSH_LIST:
                # TODO: Type check lists
                stack_push(stack, "ptr")

                # assert isinstance(op.value, list), "Expected list"
                #
                # # Empty list
                # if not op.value:
                #     stack_push(stack, "list")
                #     print(stack)
                #     continue
                #
                # # Non-empty list
                # kind = None
                # for item in op.value:
                #     assert isinstance(item, Op), "Expected Op"
                #
                #     # TODO: Support identifiers
                #     if item.kind not in LITERAL_OP_KINDS:
                #         raise SyntaxError(f"Expected literal but got `{item.kind}`")
                #     if not kind:
                #         kind = item.kind
                #         continue
                #
                #     if item.kind != kind:
                #         raise SyntaxError(
                #             f"Expected list of `{kind}` but found `{item.kind}`"
                #         )
                #
                # assert kind, "Kind should be known by now"
                # match kind:
                #     case OpKind.PUSH_INT:
                #         stack_push(stack, "list[int]")
                #     case _:
                #         SyntaxError(f"`{kind}` is not a literal kind")

            case OpKind.PRINT:
                stack_pop(stack)
            case OpKind.PUSH_INT:
                stack_push(stack, "int")
            case OpKind.ROT:
                a = stack_pop(stack)
                b = stack_pop(stack)
                c = stack_pop(stack)
                stack_push(stack, c)
                stack_push(stack, a)
                stack_push(stack, b)
            case OpKind.STORE:
                expect_type(stack, "ptr")
                expect_type(stack, "any")
            case OpKind.SWAP:
                a = stack_pop(stack)
                b = stack_pop(stack)
                stack_push(stack, a)
                stack_push(stack, b)
            case _:
                assert_never(op.kind)


def stack_push(stack: list[Type], typ: Type):
    stack.append(typ)


def stack_peek(stack: list[Type]) -> Type | None:
    if not stack:
        return None
    return stack[-1]


def stack_pop(stack: list[Type]) -> Type:
    if not stack:
        raise IndexError("Stack underflow")
    return stack.pop()


def expect_type(stack: list[Type], expected: Type) -> Type:
    typ = stack_pop(stack)
    if expected == "any" or typ == "any":
        return typ
    if typ != expected:
        raise ValueError(f"Expected `{expected}` but got `{typ}`")
    return typ
