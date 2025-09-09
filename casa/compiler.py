from collections.abc import Iterable
from dataclasses import dataclass, field
import random
from typing import assert_never

from casa.common import (
    GLOBAL_IDENTIFIERS,
    Bytecode,
    Cursor,
    Function,
    Inst,
    InstKind,
    LabelId,
    Op,
    OpKind,
)


def compile_bytecode(ops: list[Op]) -> Bytecode:
    compiler = Compiler(ops)
    return compiler.compile()


def op_to_label(op: Op) -> LabelId:
    return id(op)


@dataclass
class Compiler:
    ops: list[Op]
    labels: set[LabelId] = field(default_factory=set)
    locals: list[str] = field(default_factory=list)

    def compile(self) -> Bytecode:
        assert len(InstKind) == 25, "Exhaustive handling for `InstructionKind"
        assert len(OpKind) == 29, "Exhaustive handling for `OpKind`"

        cursor = Cursor(sequence=self.ops)
        bytecode = []
        while op := cursor.pop():
            match op.kind:
                case OpKind.ADD:
                    bytecode.append(Inst(InstKind.ADD))
                case OpKind.ASSIGN_DECREMENT:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"
                    assert variable_name in self.locals, "Variable is defined"

                    index = self.locals.index(variable_name)
                    bytecode.append(Inst(InstKind.LOCAL_GET, arguments=[index]))
                    bytecode.append(Inst(InstKind.SWAP))
                    bytecode.append(Inst(InstKind.SUB))
                    bytecode.append(Inst(InstKind.LOCAL_SET, arguments=[index]))
                case OpKind.ASSIGN_INCREMENT:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"
                    assert variable_name in self.locals, "Variable is defined"

                    index = self.locals.index(variable_name)
                    bytecode.append(Inst(InstKind.LOCAL_GET, arguments=[index]))
                    bytecode.append(Inst(InstKind.ADD))
                    bytecode.append(Inst(InstKind.LOCAL_SET, arguments=[index]))
                case OpKind.ASSIGN_VARIABLE:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"

                    if variable_name not in self.locals:
                        self.locals.append(variable_name)
                    index = self.locals.index(variable_name)
                    bytecode.append(Inst(InstKind.LOCAL_SET, arguments=[index]))
                case OpKind.CALL_FN:
                    function_name = op.value
                    function = GLOBAL_IDENTIFIERS.get(function_name)
                    assert isinstance(function, Function), "Expected function"

                    # Compile the function if it is not compiled already
                    if function.bytecode is None:
                        function.bytecode = compile_bytecode(function.ops)
                    bytecode.append(Inst(InstKind.CALL_FN, arguments=[function_name]))
                case OpKind.SUB:
                    bytecode.append(Inst(InstKind.SUB))
                case OpKind.DROP:
                    bytecode.append(Inst(InstKind.DROP))
                case OpKind.DUP:
                    bytecode.append(Inst(InstKind.DUP))
                case OpKind.EQ:
                    bytecode.append(Inst(InstKind.EQ))
                case OpKind.EXEC_FN:
                    bytecode.append(Inst(InstKind.EXEC_FN))
                case OpKind.GE:
                    bytecode.append(Inst(InstKind.GE))
                case OpKind.GT:
                    bytecode.append(Inst(InstKind.GT))
                case OpKind.IDENTIFIER:
                    raise AssertionError(
                        f"Identifier `{op.value}` should be resolved by the parser"
                    )
                case OpKind.LE:
                    bytecode.append(Inst(InstKind.LE))
                case OpKind.LOAD:
                    bytecode.append(Inst(InstKind.LOAD))
                case OpKind.LT:
                    bytecode.append(Inst(InstKind.LT))
                case OpKind.NE:
                    bytecode.append(Inst(InstKind.NE))
                case OpKind.OVER:
                    bytecode.append(Inst(InstKind.OVER))
                case OpKind.PRINT:
                    bytecode.append(Inst(InstKind.PRINT))
                case OpKind.PUSH_INT:
                    bytecode.append(Inst(InstKind.PUSH, arguments=[op.value]))
                case OpKind.PUSH_FN:
                    for i, (name, function) in enumerate(GLOBAL_IDENTIFIERS.items()):
                        if name == op.value:
                            assert isinstance(
                                function, Function
                            ), "Expected lambda function"
                            function.bytecode = compile_bytecode(function.ops)
                            bytecode.append(Inst(InstKind.PUSH, arguments=[i]))
                    raise NameError(f"Function `{op.value}` is not defined")
                case OpKind.PUSH_LIST:
                    assert isinstance(op.value, list), "Expected `list`"

                    # Push list items in the reverse order
                    reversed_items: list[Op] = list(reversed(op.value))
                    list_bytecode = compile_bytecode(reversed_items)

                    # First item of the list is its length
                    push_len = Inst(InstKind.PUSH, arguments=[len(op.value)])
                    list_bytecode.append(push_len)

                    # Create the list
                    push_list = Inst(InstKind.LIST_NEW)
                    list_bytecode.append(push_list)

                    bytecode += list_bytecode
                case OpKind.PUSH_VARIABLE:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"
                    if variable_name not in self.locals:
                        raise NameError(f"Variable `{variable_name}` does not exist")

                    index = self.locals.index(variable_name)
                    bytecode.append(Inst(InstKind.LOCAL_GET, arguments=[index]))
                case OpKind.ROT:
                    bytecode.append(Inst(InstKind.ROT))
                case OpKind.STORE:
                    bytecode.append(Inst(InstKind.STORE))
                case OpKind.SWAP:
                    bytecode.append(Inst(InstKind.SWAP))
                case OpKind.WHILE_CONDITION:
                    # Find matching `WHILE_END`
                    end_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                    )
                    bytecode.append(Inst(InstKind.JUMP_IF, arguments=[end_label]))
                case OpKind.WHILE_END:
                    # Add label
                    label = op_to_label(op)
                    self.add_label(label)

                    # Find matching `WHILE_START`
                    start_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                        reverse=True,
                    )
                    bytecode.append(Inst(InstKind.JUMP, arguments=[start_label]))
                    bytecode.append(Inst(InstKind.LABEL, arguments=[label]))
                case OpKind.WHILE_START:
                    label = op_to_label(op)
                    bytecode.append(Inst(InstKind.LABEL, arguments=[label]))
                case _:
                    assert_never(op.kind)

        return bytecode

    def add_label(self, label_id: LabelId | None) -> LabelId:
        if not label_id:
            label_id = random.getrandbits(128)

        assert label_id not in self.labels, "Expected unique label"
        self.labels.add(label_id)
        return label_id

    def find_matching_label(
        self,
        op: Op,
        start_kind: OpKind,
        end_kind: OpKind,
        reverse: bool = False,
    ) -> LabelId:
        op_index = self.ops.index(op)
        iterable: Iterable[Op] = (
            reversed(self.ops[:op_index]) if reverse else self.ops[op_index + 1 :]
        )

        depth = 1
        direction = -1 if reverse else 1
        for other_op in iterable:
            if other_op.kind == start_kind:
                depth += direction
            elif other_op.kind == end_kind:
                depth -= direction

            if depth == 0:
                return op_to_label(other_op)

        raise ValueError(f"Matching {start_kind if reverse else end_kind} not found")
