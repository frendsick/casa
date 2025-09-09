from typing import assert_never

from casa.common import (
    GLOBAL_IDENTIFIERS,
    Bytecode,
    Function,
    InstructionKind,
    LabelId,
)

InstrAddr = int


def interpret_bytecode(bytecode: Bytecode, stack: list[int] | None = None):
    assert len(InstructionKind) == 25, "Exhaustive handling for `InstructionKind`"

    # Containers for emulating a computer
    heap: list[int] = []
    labels: dict[LabelId, InstrAddr] = {}
    locals: list[int] = []
    if not stack:
        stack = []

    # Set up the program
    for instr_addr, instruction in enumerate(bytecode):
        if instruction.kind == InstructionKind.LABEL:
            assert len(instruction.arguments) == 1, "Label ID"
            label_id = instruction.arguments[0]
            assert isinstance(label_id, LabelId), ""
            labels[label_id] = instr_addr

    # Interpret the program
    pc = -1  # Program counter
    while (pc := pc + 1) < len(bytecode):
        instruction = bytecode[pc]
        match instruction.kind:
            case InstructionKind.ADD:
                a = stack_pop(stack)
                b = stack_pop(stack)
                stack_push(stack, b + a)
            case InstructionKind.CALL_FN:
                assert len(instruction.arguments) == 1, "Function name"
                function_name = instruction.arguments[0]
                function = GLOBAL_IDENTIFIERS.get(function_name)

                assert isinstance(function, Function), "Expected function"
                assert isinstance(function.bytecode, list), "Function is compiled"

                interpret_bytecode(function.bytecode, stack)
            case InstructionKind.SUB:
                a = stack_pop(stack)
                b = stack_pop(stack)
                stack_push(stack, b - a)
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
            case InstructionKind.JUMP:
                label = instruction.arguments[0]
                pc = labels[label]
            case InstructionKind.JUMP_IF:
                condition = stack_pop(stack)
                if condition == int(False):
                    label: LabelId = instruction.arguments[0]
                    pc = labels[label]
            case InstructionKind.LABEL:
                label: LabelId = instruction.arguments[0]
                assert label in labels, f"Label `{label}` does not exist"
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
            case InstructionKind.LOCAL_GET:
                assert len(instruction.arguments) == 1, "Local index"
                index = instruction.arguments[0]
                assert isinstance(index, int), "Valid index"
                assert index < len(locals), "Local should be set"

                value = locals[index]
                stack_push(stack, value)
            case InstructionKind.LOCAL_SET:
                assert len(instruction.arguments) == 1, "Local index"
                index = instruction.arguments[0]
                assert isinstance(index, int), "Valid index"

                a = stack_pop(stack)

                # Extend locals if needed
                if index >= len(locals):
                    zeroes = [0] * (index - len(locals) + 1)
                    locals.extend(zeroes)
                locals[index] = a
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
