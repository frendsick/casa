from dataclasses import dataclass
from typing import assert_never

from casa.common import GLOBAL_FUNCTIONS, Bytecode, Function, InstKind, LabelId

InstrAddr = int


@dataclass
class VirtualMachine:
    call_stack: list[int]
    constants: dict[str, int]
    data_stack: list[int]
    globals: list[int]
    heap: list[int]
    strings: dict[LabelId, str]

    def __init__(self):
        self.call_stack = []
        self.constants = {}
        self.data_stack = []
        self.globals = []
        self.heap = []
        self.strings = {}

    def heap_alloc(self, size: int) -> int:
        ptr = len(self.heap)
        for _ in range(size):
            self.heap.append(0)
        return ptr


def interpret_bytecode(
    bytecode: Bytecode,
    vm: VirtualMachine | None = None,
):
    assert len(InstKind) == 40, "Exhaustive handling for `InstructionKind`"

    if not vm:
        vm = VirtualMachine()
    is_global_scope = not vm.call_stack

    # Containers for emulating a computer
    labels: dict[LabelId, InstrAddr] = {}

    # Set up the program
    for instr_addr, instruction in enumerate(bytecode):
        if instruction.kind == InstKind.LABEL:
            assert len(instruction.args) == 1, "Label ID"
            label_id = instruction.args[0]
            assert isinstance(label_id, LabelId), ""
            labels[label_id] = instr_addr

    # Interpret the program
    pc = -1  # Program counter
    while (pc := pc + 1) < len(bytecode):
        instruction = bytecode[pc]
        match instruction.kind:
            case InstKind.ADD:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, b + a)
            case InstKind.AND:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(bool(a and b)))
            case InstKind.CONSTANT_LOAD:
                assert len(instruction.args) == 1, "Constant label"
                constant_label: str = instruction.args[0]

                value = vm.constants.get(constant_label)
                if not value:
                    raise AssertionError("Constant should be stored")
                stack_push(vm.data_stack, value)
            case InstKind.CONSTANT_STORE:
                assert len(instruction.args) == 1, "Constant label"
                constant_label: str = instruction.args[0]

                a = stack_pop(vm.data_stack)
                vm.constants[constant_label] = a
            case InstKind.DIV:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                if a == 0:
                    ZeroDivisionError("Cannot divide by zero")
                stack_push(vm.data_stack, b // a)
            case InstKind.DROP:
                stack_pop(vm.data_stack)
            case InstKind.DUP:
                a = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, a)
                stack_push(vm.data_stack, a)
            case InstKind.EQ:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(a == b))
            case InstKind.FN_CALL:
                assert len(instruction.args) == 1, "Function name"
                function_name = instruction.args[0]
                function = GLOBAL_FUNCTIONS.get(function_name)

                assert isinstance(function, Function), "Expected function"
                assert isinstance(function.bytecode, list), "Function is compiled"

                vm.call_stack.append(pc)
                interpret_bytecode(function.bytecode, vm)
            case InstKind.FN_EXEC:
                fn_ptr = stack_pop(vm.data_stack)
                assert fn_ptr < len(GLOBAL_FUNCTIONS), "Valid function pointer"

                function = list(GLOBAL_FUNCTIONS.values())[fn_ptr]
                assert isinstance(function.bytecode, list), "Function is compiled"

                vm.call_stack.append(pc)
                interpret_bytecode(function.bytecode, vm)
            case InstKind.FN_RETURN:
                if is_global_scope:
                    return
                pc = stack_pop(vm.call_stack)
                return
            case InstKind.GE:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(a >= b))
            case InstKind.GLOBAL_GET:
                assert len(instruction.args) == 1, "Global index"
                index = instruction.args[0]
                assert isinstance(index, int), "Valid index"
                assert index < len(vm.globals), "Global should be set"

                value = vm.globals[index]
                stack_push(vm.data_stack, value)
            case InstKind.GLOBAL_SET:
                assert len(instruction.args) == 1, "Global index"
                index = instruction.args[0]
                assert isinstance(index, int), "Valid index"

                a = stack_pop(vm.data_stack)
                assert index < len(vm.globals), "Valid global index"

                vm.globals[index] = a
            case InstKind.GLOBALS_INIT:
                assert len(instruction.args) == 1, "Globals count"
                globals_count = instruction.args[0]
                assert isinstance(globals_count, int), "Valid globals count"
                vm.globals = [0] * globals_count
            case InstKind.GT:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(a > b))
            case InstKind.HEAP_ALLOC:
                assert len(instruction.args) == 1, "Allocated bytes"
                allocated_bytes = instruction.args[0]
                ptr = vm.heap_alloc(allocated_bytes)
                stack_push(vm.data_stack, ptr)
            case InstKind.JUMP:
                label = instruction.args[0]
                pc = labels[label]
            case InstKind.JUMP_NE:
                condition = stack_pop(vm.data_stack)
                if condition == int(False):
                    label = instruction.args[0]
                    assert isinstance(label, LabelId), "Valid label ID"
                    pc = labels[label]
            case InstKind.LABEL:
                label: LabelId = instruction.args[0]
                assert label in labels, f"Label `{label}` does not exist"
            case InstKind.LE:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(a <= b))
            case InstKind.LOAD:
                ptr = stack_pop(vm.data_stack)
                if not is_valid_address(vm.heap, ptr):
                    raise IndexError(
                        f"Address `{ptr}` is not valid within the vm.heap of size `{len(vm.heap)}`"
                    )
                stack_push(vm.data_stack, vm.heap[ptr])
            case InstKind.LOCALS_INIT:
                assert len(instruction.args) == 1, "Locals count"
                locals_count = instruction.args[0]
                assert isinstance(locals_count, int), "Valid local count"

                for _ in range(locals_count):
                    stack_push(vm.call_stack, 0)
            case InstKind.LOCALS_UNINIT:
                assert len(instruction.args) == 1, "Locals count"
                locals_count = instruction.args[0]
                assert isinstance(locals_count, int), "Valid local count"

                for _ in range(locals_count):
                    stack_pop(vm.call_stack)
            case InstKind.LOCAL_GET:
                assert len(instruction.args) == 1, "Local index"
                index = instruction.args[0]
                assert isinstance(index, int), "Valid index"

                value = vm.call_stack[-index - 1]
                stack_push(vm.data_stack, value)
            case InstKind.LOCAL_SET:
                assert len(instruction.args) == 1, "Local index"
                index = instruction.args[0]
                assert isinstance(index, int), "Valid index"

                a = stack_pop(vm.data_stack)
                vm.call_stack[-index - 1] = a
            case InstKind.LT:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(a < b))
            case InstKind.MOD:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                if a == 0:
                    ZeroDivisionError("Cannot modulo by zero")
                stack_push(vm.data_stack, b % a)
            case InstKind.MUL:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, b * a)
            case InstKind.NE:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(a != b))
            case InstKind.NOT:
                a = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(bool(not a)))
            case InstKind.OR:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(bool(a or b)))
            case InstKind.OVER:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, b)
                stack_push(vm.data_stack, a)
                stack_push(vm.data_stack, b)
            case InstKind.PRINT:
                a = stack_pop(vm.data_stack)
                if string := vm.strings.get(a):
                    print(string)
                else:
                    print(a)
            case InstKind.PUSH:
                stack_push(vm.data_stack, instruction.args[0])
            case InstKind.PUSH_STR:
                assert len(instruction.args) == 1, "String literal"
                string = instruction.args[0]
                assert isinstance(string, str), "Valid string literal"

                label = id(string)
                vm.strings[label] = string
                stack_push(vm.data_stack, label)
            case InstKind.ROT:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                c = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, c)
                stack_push(vm.data_stack, a)
                stack_push(vm.data_stack, b)
            case InstKind.STORE:
                ptr = stack_pop(vm.data_stack)
                if not is_valid_address(vm.heap, ptr):
                    raise IndexError(
                        f"Address `{ptr}` is not valid within the vm.heap of size `{len(vm.heap)}`"
                    )
                value = stack_pop(vm.data_stack)
                vm.heap[ptr] = value
            case InstKind.SUB:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, b - a)
            case InstKind.SWAP:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, a)
                stack_push(vm.data_stack, b)
            case _:
                assert_never(instruction.kind)


def stack_push(stack: list[int], value: int):
    stack.append(value)


def stack_pop(stack: list[int]) -> int:
    if not stack:
        raise IndexError("Stack underflow")
    return stack.pop()


def is_valid_address(heap: list[int], ptr: int) -> bool:
    return ptr < len(heap) and ptr >= 0
