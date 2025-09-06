from dataclasses import dataclass, field
from enum import Enum, auto
from typing import assert_never

from casa.common import Cursor, Op, OpKind


class InstructionKind(Enum):
    ADD = auto()
    # LIST_LEN = auto()
    LIST_NEW = auto()
    PRINT = auto()
    PUSH = auto()


@dataclass
class Instruction:
    kind: InstructionKind
    arguments: list = field(default_factory=list)

    def __post_init__(self):
        assert len(InstructionKind) == 4, "Exhaustive handling for `InstructionKind`"
        match self.kind:
            # Should not have a parameter
            case (
                InstructionKind.ADD
                # | InstructionKind.LIST_LEN
                | InstructionKind.LIST_NEW
                | InstructionKind.PRINT
            ):
                if self.arguments:
                    raise TypeError(
                        f"`{self.kind}` should not have any parameters\nArguments: {self.arguments}"
                    )
            # One parameter of type `int`
            case InstructionKind.PUSH:
                if len(self.arguments) != 1 or not isinstance(self.arguments[0], int):
                    raise TypeError(
                        f"`{self.kind}` requires one parameter of type `int`\nArguments: {self.arguments}"
                    )


def compile_bytecode(ops: list[Op]) -> list[Instruction]:
    cursor = Cursor(sequence=ops)
    instructions = []
    while op := cursor.pop():
        instructions += compile_op(op)
    return instructions


def compile_op(op: Op) -> list[Instruction]:
    assert len(InstructionKind) == 4, "Exhaustive handling for `InstructionKind"
    assert len(OpKind) == 4, "Exhaustive handling for `OpKind`"

    match op.kind:
        case OpKind.ADD:
            return [Instruction(InstructionKind.ADD)]
        case OpKind.PRINT:
            return [Instruction(InstructionKind.PRINT)]
        case OpKind.PUSH_INT:
            return [Instruction(InstructionKind.PUSH, arguments=[op.value])]
        case OpKind.PUSH_LIST:
            assert isinstance(op.value, list), "Expected `list`"

            instructions = []
            for value in op.value:
                assert isinstance(value, Op), "Expected `Op` as value in list"
                instructions += compile_op(value)

            push_len = Instruction(InstructionKind.PUSH, arguments=[len(op.value)])
            instructions.append(push_len)
            push_list = Instruction(InstructionKind.LIST_NEW)
            instructions.append(push_list)
            return instructions
        case _:
            assert_never(op.kind)
