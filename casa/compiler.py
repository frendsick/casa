import builtins
from typing import assert_never

from casa.common import (
    GLOBAL_IDENTIFIERS,
    Bytecode,
    Cursor,
    Function,
    Instruction,
    InstructionKind,
    Op,
    OpKind,
)


def compile_bytecode(ops: list[Op]) -> Bytecode:
    cursor = Cursor(sequence=ops)
    bytecode = []
    while op := cursor.pop():
        bytecode += compile_op(op)
    return bytecode


def compile_op(op: Op) -> Bytecode:
    assert len(InstructionKind) == 19, "Exhaustive handling for `InstructionKind"
    assert len(OpKind) == 21, "Exhaustive handling for `OpKind`"

    match op.kind:
        case OpKind.ADD:
            return [Instruction(InstructionKind.ADD)]
        case OpKind.CALL_FN:
            function_name = op.value
            function = GLOBAL_IDENTIFIERS.get(function_name)
            assert isinstance(function, Function), "Expected function"

            # Compile the function if it is not compiled already
            if function.bytecode is None:
                function.bytecode = compile_bytecode(function.ops)
            return [Instruction(InstructionKind.CALL_FN, arguments=[function_name])]
        case OpKind.DROP:
            return [Instruction(InstructionKind.DROP)]
        case OpKind.DUP:
            return [Instruction(InstructionKind.DUP)]
        case OpKind.EQ:
            return [Instruction(InstructionKind.EQ)]
        case OpKind.EXEC_FN:
            return [Instruction(InstructionKind.EXEC_FN)]
        case OpKind.GE:
            return [Instruction(InstructionKind.GE)]
        case OpKind.GT:
            return [Instruction(InstructionKind.GT)]
        case OpKind.IDENTIFIER:
            raise AssertionError(
                f"Identifier `{op.value}` should be resolved by the parser"
            )
        case OpKind.LE:
            return [Instruction(InstructionKind.LE)]
        case OpKind.LOAD:
            return [Instruction(InstructionKind.LOAD)]
        case OpKind.LT:
            return [Instruction(InstructionKind.LT)]
        case OpKind.NE:
            return [Instruction(InstructionKind.NE)]
        case OpKind.OVER:
            return [Instruction(InstructionKind.OVER)]
        case OpKind.PRINT:
            return [Instruction(InstructionKind.PRINT)]
        case OpKind.PUSH_INT:
            return [Instruction(InstructionKind.PUSH, arguments=[op.value])]
        case OpKind.PUSH_FN:
            for i, (name, function) in enumerate(GLOBAL_IDENTIFIERS.items()):
                if name == op.value:
                    assert isinstance(function, Function), "Expected lambda function"
                    function.bytecode = compile_bytecode(function.ops)
                    return [Instruction(InstructionKind.PUSH, arguments=[i])]
            raise NameError(f"Function `{op.value}` is not defined")
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
