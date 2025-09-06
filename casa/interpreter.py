from typing import assert_never

from casa.compiler import Instruction, InstructionKind


def interpret_bytecode(instructions: list[Instruction]):
    heap: list[int] = []
    stack: list[int] = []
    for instruction in instructions:
        interpret_instruction(instruction, stack, heap)


def interpret_instruction(instruction: Instruction, stack: list[int], heap: list[int]):
    assert len(InstructionKind) == 11, "Exhaustive handling for `InstructionKind`"

    match instruction.kind:
        case InstructionKind.ADD:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, a + b)
        case InstructionKind.DROP:
            stack_pop(stack)
        case InstructionKind.DUP:
            a = stack_pop(stack)
            stack_push(stack, a)
            stack_push(stack, a)
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
    return stack.pop()


def heap_alloc(heap: list[int], size: int) -> int:
    ptr = len(heap)
    for _ in range(size):
        heap.append(0)
    return ptr


def is_valid_address(heap: list[int], ptr: int) -> bool:
    return ptr < len(heap) and ptr >= 0
