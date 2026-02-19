from dataclasses import dataclass
from typing import assert_never

from casa.common import GLOBAL_FUNCTIONS, Bytecode, Function, InstKind, LabelId, Program

InstrAddr = int
STRING_TAG = 1 << 60


@dataclass
class VirtualMachine:
    call_stack: list[int]
    constants: list[int]
    data_stack: list[int]
    globals: list[int]
    heap: list[int]
    strings: list[str]

    def __init__(self, strings: list[str] | None = None, constants_count: int = 0):
        self.call_stack = []
        self.constants = [0] * constants_count
        self.data_stack = []
        self.globals = []
        self.heap = []
        self.strings = strings or []

    def heap_alloc(self, size: int) -> int:
        ptr = len(self.heap)
        for _ in range(size):
            self.heap.append(0)
        return ptr


def interpret_program(program: Program):
    vm = VirtualMachine(
        strings=program.strings,
        constants_count=program.constants_count,
    )
    interpret_bytecode(program.bytecode, vm)


def interpret_bytecode(
    bytecode: Bytecode,
    vm: VirtualMachine | None = None,
):
    assert len(InstKind) == 42, "Exhaustive handling for `InstructionKind`"

    if not vm:
        vm = VirtualMachine()
    is_global_scope = not vm.call_stack

    # Containers for emulating a computer
    labels: dict[LabelId, InstrAddr] = {}

    # Set up the program
    for instr_addr, instruction in enumerate(bytecode):
        if instruction.kind == InstKind.LABEL:
            labels[instruction.int_arg] = instr_addr

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
                stack_push(vm.data_stack, vm.constants[instruction.int_arg])
            case InstKind.CONSTANT_STORE:
                a = stack_pop(vm.data_stack)
                vm.constants[instruction.int_arg] = a
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
                function = GLOBAL_FUNCTIONS.get(instruction.str_arg)

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
                index = instruction.int_arg
                assert index < len(vm.globals), "Global should be set"
                stack_push(vm.data_stack, vm.globals[index])
            case InstKind.GLOBAL_SET:
                index = instruction.int_arg
                a = stack_pop(vm.data_stack)
                assert index < len(vm.globals), "Valid global index"
                vm.globals[index] = a
            case InstKind.GLOBALS_INIT:
                vm.globals = [0] * instruction.int_arg
            case InstKind.GT:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, int(a > b))
            case InstKind.HEAP_ALLOC:
                allocated_bytes = stack_pop(vm.data_stack)
                ptr = vm.heap_alloc(allocated_bytes)
                stack_push(vm.data_stack, ptr)
            case InstKind.JUMP:
                pc = labels[instruction.int_arg]
            case InstKind.JUMP_NE:
                condition = stack_pop(vm.data_stack)
                if condition == int(False):
                    pc = labels[instruction.int_arg]
            case InstKind.LABEL:
                assert (
                    instruction.int_arg in labels
                ), f"Label `{instruction.int_arg}` does not exist"
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
                for _ in range(instruction.int_arg):
                    stack_push(vm.call_stack, 0)
            case InstKind.LOCALS_UNINIT:
                for _ in range(instruction.int_arg):
                    stack_pop(vm.call_stack)
            case InstKind.LOCAL_GET:
                value = vm.call_stack[-instruction.int_arg - 1]
                stack_push(vm.data_stack, value)
            case InstKind.LOCAL_SET:
                a = stack_pop(vm.data_stack)
                vm.call_stack[-instruction.int_arg - 1] = a
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
                if a >= STRING_TAG:
                    string_index = a - STRING_TAG
                    print(vm.strings[string_index])
                else:
                    print(a)
            case InstKind.PUSH:
                stack_push(vm.data_stack, instruction.int_arg)
            case InstKind.PUSH_STR:
                stack_push(vm.data_stack, STRING_TAG + instruction.int_arg)
            case InstKind.ROT:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                c = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, c)
                stack_push(vm.data_stack, a)
                stack_push(vm.data_stack, b)
            case InstKind.SHL:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, b << a)
            case InstKind.SHR:
                a = stack_pop(vm.data_stack)
                b = stack_pop(vm.data_stack)
                stack_push(vm.data_stack, b >> a)
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
