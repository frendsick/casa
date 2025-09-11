from typing import assert_never

from casa.common import GLOBAL_FUNCTIONS, Bytecode, Function, InstKind, LabelId

InstrAddr = int


def interpret_bytecode(
    bytecode: Bytecode,
    call_stack: list[int] | None = None,
    data_stack: list[int] | None = None,
    globals: list[int] | None = None,
    captures: dict[str, int] | None = None,
):
    assert len(InstKind) == 40, "Exhaustive handling for `InstructionKind`"

    is_global_scope = not call_stack

    # Containers for emulating a computer
    heap: list[int] = []
    labels: dict[LabelId, InstrAddr] = {}
    locals: list[int] = []
    strings: dict[LabelId, str] = {}
    if not call_stack:
        call_stack = []
    if not data_stack:
        data_stack = []
    if not globals:
        globals = []
    if not captures:
        captures = {}

    # Set up the program
    for instr_addr, instruction in enumerate(bytecode):
        if instruction.kind == InstKind.LABEL:
            assert len(instruction.arguments) == 1, "Label ID"
            label_id = instruction.arguments[0]
            assert isinstance(label_id, LabelId), ""
            labels[label_id] = instr_addr

    # Interpret the program
    pc = -1  # Program counter
    while (pc := pc + 1) < len(bytecode):
        instruction = bytecode[pc]
        match instruction.kind:
            case InstKind.ADD:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, b + a)
            case InstKind.AND:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, int(bool(a and b)))
            case InstKind.CAPTURE_LOAD:
                assert len(instruction.arguments) == 2, "Capture name and function"
                capture_name: str = instruction.arguments[0]
                function_name: str = instruction.arguments[1]
                capture_label = f"{function_name}_{capture_name}"

                value = captures.get(capture_label)
                if not value:
                    raise AssertionError("Capture should be stored")
                stack_push(data_stack, value)
            case InstKind.CAPTURE_STORE:
                assert len(instruction.arguments) == 2, "Capture name and function"
                capture_name: str = instruction.arguments[0]
                function_name: str = instruction.arguments[1]
                capture_label = f"{function_name}_{capture_name}"

                a = stack_pop(data_stack)
                captures[capture_label] = a
            case InstKind.DIV:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                if a == 0:
                    ZeroDivisionError("Cannot divide by zero")
                stack_push(data_stack, b // a)
            case InstKind.DROP:
                stack_pop(data_stack)
            case InstKind.DUP:
                a = stack_pop(data_stack)
                stack_push(data_stack, a)
                stack_push(data_stack, a)
            case InstKind.EQ:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, int(a == b))
            case InstKind.FN_CALL:
                assert len(instruction.arguments) == 1, "Function name"
                function_name = instruction.arguments[0]
                function = GLOBAL_FUNCTIONS.get(function_name)

                assert isinstance(function, Function), "Expected function"
                assert isinstance(function.bytecode, list), "Function is compiled"

                call_stack.append(pc)
                interpret_bytecode(
                    function.bytecode, call_stack, data_stack, globals, captures
                )
            case InstKind.FN_EXEC:
                fn_ptr = stack_pop(data_stack)
                assert fn_ptr < len(GLOBAL_FUNCTIONS), "Valid function pointer"

                function = list(GLOBAL_FUNCTIONS.values())[fn_ptr]
                assert isinstance(function.bytecode, list), "Function is compiled"

                call_stack.append(pc)
                interpret_bytecode(
                    function.bytecode, call_stack, data_stack, globals, captures
                )
            case InstKind.FN_RETURN:
                if is_global_scope:
                    return
                pc = stack_pop(call_stack)
                return
            case InstKind.GE:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, int(a >= b))
            case InstKind.GLOBAL_GET:
                assert len(instruction.arguments) == 1, "Global index"
                index = instruction.arguments[0]
                assert isinstance(index, int), "Valid index"
                assert index < len(globals), "Global should be set"

                value = globals[index]
                stack_push(data_stack, value)
            case InstKind.GLOBAL_SET:
                assert len(instruction.arguments) == 1, "Global index"
                index = instruction.arguments[0]
                assert isinstance(index, int), "Valid index"

                a = stack_pop(data_stack)
                assert index < len(globals), "Valid global index"

                globals[index] = a
            case InstKind.GLOBALS_INIT:
                assert len(instruction.arguments) == 1, "Globals count"
                globals_count = instruction.arguments[0]
                assert isinstance(globals_count, int), "Valid globals count"
                globals = [0] * globals_count
            case InstKind.GT:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, int(a > b))
            case InstKind.JUMP:
                label = instruction.arguments[0]
                pc = labels[label]
            case InstKind.JUMP_NE:
                condition = stack_pop(data_stack)
                if condition == int(False):
                    label = instruction.arguments[0]
                    assert isinstance(label, LabelId), "Valid label ID"
                    pc = labels[label]
            case InstKind.LABEL:
                label: LabelId = instruction.arguments[0]
                assert label in labels, f"Label `{label}` does not exist"
            case InstKind.LE:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, int(a <= b))
            case InstKind.LIST_NEW:
                list_len = stack_pop(data_stack)

                # Store the list len in the zeroth index
                ptr = heap_alloc(heap, list_len + 1)
                heap[ptr] = list_len

                # Store the list values
                for i in range(1, list_len + 1):
                    item = stack_pop(data_stack)
                    heap[ptr + i] = item
                stack_push(data_stack, ptr)
            case InstKind.LOAD:
                ptr = stack_pop(data_stack)
                if not is_valid_address(heap, ptr):
                    raise IndexError(
                        f"Address `{ptr}` is not valid within the heap of size `{len(heap)}`"
                    )
                stack_push(data_stack, heap[ptr])
            case InstKind.LOCALS_INIT:
                assert len(instruction.arguments) == 1, "Locals count"
                locals_count = instruction.arguments[0]
                assert isinstance(locals_count, int), "Valid local count"

                for _ in range(locals_count):
                    stack_push(call_stack, 0)
            case InstKind.LOCALS_UNINIT:
                assert len(instruction.arguments) == 1, "Locals count"
                locals_count = instruction.arguments[0]
                assert isinstance(locals_count, int), "Valid local count"

                for _ in range(locals_count):
                    stack_pop(call_stack)
            case InstKind.LOCAL_GET:
                assert len(instruction.arguments) == 1, "Local index"
                index = instruction.arguments[0]
                assert isinstance(index, int), "Valid index"
                assert index < len(locals), "Local should be set"

                value = locals[index]
                stack_push(data_stack, value)
            case InstKind.LOCAL_SET:
                assert len(instruction.arguments) == 1, "Local index"
                index = instruction.arguments[0]
                assert isinstance(index, int), "Valid index"

                a = stack_pop(data_stack)

                # Extend locals if needed
                if index >= len(locals):
                    zeroes = [0] * (index - len(locals) + 1)
                    locals.extend(zeroes)
                locals[index] = a
            case InstKind.LT:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, int(a < b))
            case InstKind.MOD:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                if a == 0:
                    ZeroDivisionError("Cannot modulo by zero")
                stack_push(data_stack, b % a)
            case InstKind.MUL:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, b * a)
            case InstKind.NE:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, int(a != b))
            case InstKind.NOT:
                a = stack_pop(data_stack)
                stack_push(data_stack, int(bool(not a)))
            case InstKind.OR:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, int(bool(a or b)))
            case InstKind.OVER:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, b)
                stack_push(data_stack, a)
                stack_push(data_stack, b)
            case InstKind.PRINT:
                a = stack_pop(data_stack)
                if string := strings.get(a):
                    print(string)
                else:
                    print(a)
            case InstKind.PUSH:
                stack_push(data_stack, instruction.arguments[0])
            case InstKind.PUSH_STR:
                assert len(instruction.arguments) == 1, "String literal"
                string = instruction.arguments[0]
                assert isinstance(string, str), "Valid string literal"

                label = id(string)
                strings[label] = string
                stack_push(data_stack, label)
            case InstKind.ROT:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                c = stack_pop(data_stack)
                stack_push(data_stack, c)
                stack_push(data_stack, a)
                stack_push(data_stack, b)
            case InstKind.STORE:
                ptr = stack_pop(data_stack)
                if not is_valid_address(heap, ptr):
                    raise IndexError(
                        f"Address `{ptr}` is not valid within the heap of size `{len(heap)}`"
                    )
                value = stack_pop(data_stack)
                heap[ptr] = value
            case InstKind.SUB:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, b - a)
            case InstKind.SWAP:
                a = stack_pop(data_stack)
                b = stack_pop(data_stack)
                stack_push(data_stack, a)
                stack_push(data_stack, b)
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
