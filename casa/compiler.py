from dataclasses import dataclass, field
from enum import Enum, auto
from typing import assert_never

from casa.common import Op, OpKind


class InstructionKind(Enum):
    ADD = auto()
    PRINT = auto()
    PUSH_INT = auto()


@dataclass
class Instruction:
    kind: InstructionKind
    arguments: list = field(default_factory=list)

    def __post_init__(self):
        assert len(InstructionKind) == 3, "Exhaustive handling for `InstructionKind`"
        match self.kind:
            # Should not have a parameter
            case InstructionKind.ADD | InstructionKind.PRINT:
                if self.arguments:
                    raise TypeError(
                        f"`{self.kind}` should not have any parameters\nArguments: {self.arguments}"
                    )
            # One parameter of type `int`
            case InstructionKind.PUSH_INT:
                if len(self.arguments) != 1 or not isinstance(self.arguments[0], int):
                    raise TypeError(
                        f"`{self.kind}` requires one parameter of type `int`\nArguments: {self.arguments}"
                    )


@dataclass
class BytecodeCompiler:
    cursor: int
    ops: list[Op]

    def compile(self) -> list[Instruction]:
        instructions = []
        while instruction := self.parse_instruction():
            instructions.append(instruction)
        return instructions

    def is_finished(self) -> bool:
        return self.cursor >= len(self.ops)

    def peek_instruction(self) -> Instruction | None:
        assert len(InstructionKind) == 3, "Exhaustive handling for `InstructionKind"

        if self.is_finished():
            return None

        op = self.ops[self.cursor]
        match op.kind:
            case OpKind.ADD:
                return Instruction(InstructionKind.ADD)
            case OpKind.PRINT:
                return Instruction(InstructionKind.PRINT)
            case OpKind.PUSH_INT:
                return Instruction(InstructionKind.PUSH_INT, arguments=[op.value])
            case _:
                assert_never(op.kind)

    def parse_instruction(self) -> Instruction | None:
        if instruction := self.peek_instruction():
            self.cursor += 1
            return instruction


def compile_bytecode(ops: list[Op]) -> list[Instruction]:
    compiler = BytecodeCompiler(cursor=0, ops=ops)
    return compiler.compile()
