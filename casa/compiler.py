from typing import assert_never

from casa.common import Bytecode, Cursor, Instruction, InstructionKind, Op, OpKind


def compile_bytecode(ops: list[Op]) -> Bytecode:
    cursor = Cursor(sequence=ops)
    bytecode = []
    while op := cursor.pop():
        bytecode += compile_op(op)
    return bytecode


def compile_op(op: Op) -> Bytecode:
    assert len(InstructionKind) == 11, "Exhaustive handling for `InstructionKind"
    assert len(OpKind) == 11, "Exhaustive handling for `OpKind`"

    match op.kind:
        case OpKind.ADD:
            return [Instruction(InstructionKind.ADD)]
        case OpKind.DROP:
            return [Instruction(InstructionKind.DROP)]
        case OpKind.DUP:
            return [Instruction(InstructionKind.DUP)]
        case OpKind.LOAD:
            return [Instruction(InstructionKind.LOAD)]
        case OpKind.OVER:
            return [Instruction(InstructionKind.OVER)]
        case OpKind.PRINT:
            return [Instruction(InstructionKind.PRINT)]
        case OpKind.PUSH_INT:
            return [Instruction(InstructionKind.PUSH, arguments=[op.value])]
        case OpKind.PUSH_LIST:
            assert isinstance(op.value, list), "Expected `list`"

            bytecode = []
            for value in reversed(op.value):
                assert isinstance(value, Op), "Expected `Op` as value in list"
                bytecode += compile_op(value)

            push_len = Instruction(InstructionKind.PUSH, arguments=[len(op.value)])
            bytecode.append(push_len)
            push_list = Instruction(InstructionKind.LIST_NEW)
            bytecode.append(push_list)
            return bytecode
        case OpKind.ROT:
            return [Instruction(InstructionKind.ROT)]
        case OpKind.STORE:
            return [Instruction(InstructionKind.STORE)]
        case OpKind.SWAP:
            return [Instruction(InstructionKind.SWAP)]
        case _:
            assert_never(op.kind)
