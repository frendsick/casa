from typing import assert_never

from casa.common import (
    GLOBAL_IDENTIFIERS,
    Bytecode,
    Function,
    Instruction,
    InstructionKind,
)


def interpret_bytecode(bytecode: Bytecode, stack: list[int] | None = None):
    heap: list[int] = []
    if not stack:
        stack = []

    for instruction in bytecode:
        interpret_instruction(instruction, stack, heap)


def interpret_instruction(
    instruction: Instruction,
    stack: list[int],
    heap: list[int],
):
    assert len(InstructionKind) == 19, "Exhaustive handling for `InstructionKind`"

    match instruction.kind:
        case InstructionKind.ADD:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, a + b)
        case InstructionKind.CALL_FN:
            assert len(instruction.arguments) == 1, "Function name"
            function_name = instruction.arguments[0]
            function = GLOBAL_IDENTIFIERS.get(function_name)

            assert isinstance(function, Function), "Expected function"
            assert isinstance(function.bytecode, list), "Function is compiled"

            interpret_bytecode(function.bytecode, stack)
        case InstructionKind.DROP:
            stack_pop(stack)
        case InstructionKind.DUP:
            a = stack_pop(stack)
            stack_push(stack, a)
            stack_push(stack, a)
        case InstructionKind.EQ:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, int(a == b))
        case InstructionKind.EXEC_FN:
            fn_ptr = stack_pop(stack)
            assert fn_ptr < len(GLOBAL_IDENTIFIERS), "Valid function pointer"

            function = list(GLOBAL_IDENTIFIERS.values())[fn_ptr]
            assert isinstance(function, Function), "Expected function"
            assert isinstance(function.bytecode, list), "Function is compiled"

            interpret_bytecode(function.bytecode, stack)
        case InstructionKind.GE:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, int(a >= b))
        case InstructionKind.GT:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, int(a > b))
        case InstructionKind.LE:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, int(a <= b))
        case InstructionKind.LIST_NEW:
            list_len = stack_pop(stack)

            # Store the list len in the zeroth index
            ptr = heap_alloc(heap, list_len + 1)
            heap[ptr] = list_len

            # Store the list values
            for i in range(1, list_len + 1):
                item = stack_pop(stack)
                heap[ptr + i] = item
            stack_push(stack, ptr)
        case InstructionKind.LOAD:
            ptr = stack_pop(stack)
            if not is_valid_address(heap, ptr):
                raise IndexError(
                    f"Address `{ptr}` is not valid within the heap of size `{len(heap)}`"
                )
            stack_push(stack, heap[ptr])
        case InstructionKind.LT:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, int(a < b))
        case InstructionKind.NE:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, int(a != b))
        case InstructionKind.OVER:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, b)
            stack_push(stack, a)
            stack_push(stack, b)
        case InstructionKind.PRINT:
            a = stack_pop(stack)
            print(a)
        case InstructionKind.PUSH:
            stack_push(stack, instruction.arguments[0])
        case InstructionKind.ROT:
            a = stack_pop(stack)
            b = stack_pop(stack)
            c = stack_pop(stack)
            stack_push(stack, c)
            stack_push(stack, a)
            stack_push(stack, b)
        case InstructionKind.STORE:
            ptr = stack_pop(stack)
            if not is_valid_address(heap, ptr):
                raise IndexError(
                    f"Address `{ptr}` is not valid within the heap of size `{len(heap)}`"
                )
            value = stack_pop(stack)
            heap[ptr] = value
        case InstructionKind.SWAP:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, a)
            stack_push(stack, b)
        case _:
            assert_never(instruction.kind)


def stack_push(stack: list[int], value: int):
    stack.append(value)


def stack_pop(stack: list[int]) -> int:
    if not stack:
        raise IndexError("Stack underflow")
    return stack.pop()


def heap_alloc(heap: list[int], size: int) -> int:
    ptr = len(heap)
    for _ in range(size):
        heap.append(0)
    return ptr


def is_valid_address(heap: list[int], ptr: int) -> bool:
    return ptr < len(heap) and ptr >= 0
