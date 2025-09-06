from typing import assert_never

from casa.compiler import Instruction, InstructionKind


def interpret_bytecode(instructions: list[Instruction]):
    stack: list[int] = []
    for instruction in instructions:
        interpret_instruction(stack, instruction)


def interpret_instruction(stack: list[int], instruction: Instruction):
    match instruction.kind:
        case InstructionKind.ADD:
            a = stack_pop(stack)
            b = stack_pop(stack)
            stack_push(stack, a + b)
        case InstructionKind.PRINT:
            a = stack_pop(stack)
            print(a)
        case InstructionKind.PUSH_INT:
            stack_push(stack, instruction.arguments[0])
        case _:
            assert_never(instruction.kind)


def stack_push(stack: list[int], value: int):
    stack.append(value)


def stack_pop(stack: list[int]) -> int:
    return stack.pop()
