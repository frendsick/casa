from collections.abc import Iterable
from dataclasses import dataclass
from typing import assert_never

from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_SCOPE_LABEL,
    GLOBAL_VARIABLES,
    Bytecode,
    Cursor,
    Function,
    Inst,
    InstKind,
    LabelId,
    Op,
    OpKind,
    Struct,
    Variable,
)


def compile_bytecode(ops: list[Op]) -> Bytecode:
    bytecode: Bytecode = []
    compiler = Compiler(ops)

    if len(GLOBAL_VARIABLES) > 0:
        bytecode.append(Inst(InstKind.GLOBALS_INIT, arguments=[len(GLOBAL_VARIABLES)]))

    bytecode += compiler.compile()
    return bytecode


def op_to_label(op: Op) -> LabelId:
    return id(op)


@dataclass
class Compiler:
    ops: list[Op]
    function: Function | None = None

    def compile(self) -> Bytecode:
        assert len(InstKind) == 40, "Exhaustive handling for `InstructionKind"
        assert len(OpKind) == 47, "Exhaustive handling for `OpKind`"

        cursor = Cursor(sequence=self.ops)
        bytecode: list[Inst] = []

        # Function label
        fn_label = id(self.ops)
        bytecode.append(Inst(InstKind.LABEL, arguments=[fn_label]))

        if self.function and self.function.variables:
            bytecode.append(
                Inst(
                    InstKind.LOCALS_INIT,
                    arguments=[len(self.function.variables)],
                )
            )

        while op := cursor.pop():
            match op.kind:
                case OpKind.ADD:
                    bytecode.append(Inst(InstKind.ADD))
                case OpKind.AND:
                    bytecode.append(Inst(InstKind.AND))
                case OpKind.ASSIGN_DECREMENT:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"

                    if variable_name in GLOBAL_VARIABLES:
                        index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
                        bytecode.append(Inst(InstKind.GLOBAL_GET, arguments=[index]))
                        bytecode.append(Inst(InstKind.SWAP))
                        bytecode.append(Inst(InstKind.ADD))
                        bytecode.append(Inst(InstKind.GLOBAL_SET, arguments=[index]))
                    elif self.function and variable_name in self.function.variables:
                        index = self.function.variables.index(Variable(variable_name))
                        bytecode.append(Inst(InstKind.LOCAL_GET, arguments=[index]))
                        bytecode.append(Inst(InstKind.SWAP))
                        bytecode.append(Inst(InstKind.ADD))
                        bytecode.append(Inst(InstKind.LOCAL_SET, arguments=[index]))
                    else:
                        raise AssertionError(
                            f"Variable `{variable_name}` is not defined"
                        )
                case OpKind.ASSIGN_INCREMENT:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"

                    if variable_name in GLOBAL_VARIABLES:
                        index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
                        bytecode.append(Inst(InstKind.GLOBAL_GET, arguments=[index]))
                        bytecode.append(Inst(InstKind.ADD))
                        bytecode.append(Inst(InstKind.GLOBAL_SET, arguments=[index]))
                    elif self.function and variable_name in self.function.variables:
                        index = self.function.variables.index(Variable(variable_name))
                        bytecode.append(Inst(InstKind.LOCAL_GET, arguments=[index]))
                        bytecode.append(Inst(InstKind.ADD))
                        bytecode.append(Inst(InstKind.LOCAL_SET, arguments=[index]))
                    else:
                        raise AssertionError(
                            f"Variable `{variable_name}` is not defined"
                        )
                case OpKind.ASSIGN_VARIABLE:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"

                    # Global variable
                    if variable_name in GLOBAL_VARIABLES:
                        index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
                        bytecode.append(Inst(InstKind.GLOBAL_SET, arguments=[index]))
                        continue

                    # Local variable
                    assert self.function, "Function exists"
                    assert variable_name in self.function.variables, "Variable exists"
                    index = self.function.variables.index(Variable(variable_name))
                    bytecode.append(Inst(InstKind.LOCAL_SET, arguments=[index]))
                case OpKind.FN_CALL:
                    function_name = op.value
                    function = GLOBAL_FUNCTIONS.get(function_name)
                    assert isinstance(function, Function), "Expected function"

                    # Compile the function if it is not compiled already
                    if function.bytecode is None:
                        # Check if the function shadowed a global variable
                        for var in function.variables:
                            if var in GLOBAL_VARIABLES:
                                raise NameError(
                                    f"Function `{function.name}` assigns a global variable `{var.name}` before it is initialized within the global scope"
                                )

                        fn_compiler = Compiler(function.ops, function)
                        function.bytecode = fn_compiler.compile()

                    bytecode.append(Inst(InstKind.FN_CALL, arguments=[function_name]))
                case OpKind.DIV:
                    bytecode.append(Inst(InstKind.DIV))
                case OpKind.DROP:
                    bytecode.append(Inst(InstKind.DROP))
                case OpKind.DUP:
                    bytecode.append(Inst(InstKind.DUP))
                case OpKind.EQ:
                    bytecode.append(Inst(InstKind.EQ))
                case OpKind.FN_EXEC:
                    bytecode.append(Inst(InstKind.FN_EXEC))
                case OpKind.FN_PUSH:
                    function_name = op.value
                    lambda_function = GLOBAL_FUNCTIONS.get(function_name)
                    if not lambda_function:
                        raise NameError(f"Function `{function_name}` is not defined")

                    # Compile the lambda function
                    fn_compiler = Compiler(lambda_function.ops, lambda_function)
                    lambda_function.bytecode = fn_compiler.compile()

                    # Setup captures
                    for index, capture in enumerate(lambda_function.captures):
                        if capture in GLOBAL_VARIABLES:
                            index = list(GLOBAL_VARIABLES.values()).index(capture)
                            bytecode.append(
                                Inst(InstKind.GLOBAL_GET, arguments=[index])
                            )
                        elif self.function and capture in self.function.variables:
                            index = self.function.variables.index(capture)
                            bytecode.append(Inst(InstKind.LOCAL_GET, arguments=[index]))
                        else:
                            raise AssertionError("Captured variable should exist")

                        bytecode.append(
                            Inst(
                                InstKind.CONSTANT_STORE,
                                arguments=[f"{lambda_function.name}_{capture.name}"],
                            )
                        )

                    # Push function pointer
                    index = list(GLOBAL_FUNCTIONS).index(function_name)
                    bytecode.append(Inst(InstKind.PUSH, arguments=[index]))

                case OpKind.FN_RETURN:
                    bytecode.append(Inst(InstKind.FN_RETURN))
                case OpKind.GE:
                    bytecode.append(Inst(InstKind.GE))
                case OpKind.GT:
                    bytecode.append(Inst(InstKind.GT))
                case OpKind.IDENTIFIER:
                    raise AssertionError(
                        f"Identifier `{op.value}` should be resolved by the parser"
                    )
                case OpKind.IF_CONDITION:
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                        reverse=True,
                    ):
                        raise SyntaxError("`then` without matching `if`")

                    end_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                        target_kinds=[OpKind.IF_ELIF, OpKind.IF_ELSE, OpKind.IF_END],
                    )
                    if not end_label:
                        raise SyntaxError("`then` without matching `fi`")

                    bytecode.append(Inst(InstKind.JUMP_NE, arguments=[end_label]))
                case OpKind.IF_ELIF:
                    # Elif must be inside if block
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                        reverse=True,
                    ):
                        raise SyntaxError(f"`elif` without parent `if`")

                    # Elif should not be after an else
                    if self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                        target_kinds=[OpKind.IF_ELSE],
                        reverse=True,
                    ):
                        raise SyntaxError(f"`elif` after `else`")

                    elif_label = op_to_label(op)
                    end_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                    )
                    if not end_label:
                        raise SyntaxError("`elif` without matching `fi`")

                    bytecode.append(Inst(InstKind.JUMP, arguments=[end_label]))
                    bytecode.append(Inst(InstKind.LABEL, arguments=[elif_label]))
                case OpKind.IF_ELSE:
                    # Else must be inside if block
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                        reverse=True,
                    ):
                        raise SyntaxError(f"`else` without parent `if`")

                    else_label = op_to_label(op)
                    end_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                    )
                    if not end_label:
                        raise SyntaxError("`else` without matching `fi`")

                    bytecode.append(Inst(InstKind.JUMP, arguments=[end_label]))
                    bytecode.append(Inst(InstKind.LABEL, arguments=[else_label]))
                case OpKind.IF_END:
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                        reverse=True,
                    ):
                        raise SyntaxError("`fi` without parent `if`")
                    label = op_to_label(op)
                    bytecode.append(Inst(InstKind.LABEL, arguments=[label]))
                case OpKind.IF_START:
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                    ):
                        raise SyntaxError("`if` without matching `fi`")
                case OpKind.LE:
                    bytecode.append(Inst(InstKind.LE))
                case OpKind.LOAD:
                    bytecode.append(Inst(InstKind.LOAD))
                case OpKind.LT:
                    bytecode.append(Inst(InstKind.LT))
                case OpKind.MOD:
                    bytecode.append(Inst(InstKind.MOD))
                case OpKind.MUL:
                    bytecode.append(Inst(InstKind.MUL))
                case OpKind.NE:
                    bytecode.append(Inst(InstKind.NE))
                case OpKind.NOT:
                    bytecode.append(Inst(InstKind.NOT))
                case OpKind.OR:
                    bytecode.append(Inst(InstKind.OR))
                case OpKind.OVER:
                    bytecode.append(Inst(InstKind.OVER))
                case OpKind.PRINT:
                    bytecode.append(Inst(InstKind.PRINT))
                case OpKind.PUSH_BOOL:
                    bytecode.append(Inst(InstKind.PUSH, arguments=[int(op.value)]))
                case OpKind.PUSH_CAPTURE:
                    capture_name = op.value
                    assert isinstance(capture_name, str), "Valid capture name"

                    function_name = (
                        self.function.name if self.function else GLOBAL_SCOPE_LABEL
                    )
                    bytecode.append(
                        Inst(
                            InstKind.CONSTANT_LOAD,
                            arguments=[f"{function_name}_{capture_name}"],
                        )
                    )
                case OpKind.PUSH_INT:
                    bytecode.append(Inst(InstKind.PUSH, arguments=[op.value]))
                case OpKind.PUSH_LIST:
                    assert isinstance(op.value, list), "Expected `list`"
                    item_count = len(op.value)

                    # Push list items in the reverse order
                    reversed_items: list[Op] = list(reversed(op.value))
                    list_bytecode = Compiler(reversed_items).compile()
                    bytecode += list_bytecode

                    # First item of the list is its length
                    push_len = Inst(InstKind.PUSH, arguments=[item_count])
                    bytecode.append(push_len)

                    # Allocate memory for the list
                    heap_alloc = Inst(InstKind.HEAP_ALLOC, arguments=[item_count + 1])
                    bytecode.append(heap_alloc)

                    # Create the list
                    for i in range(item_count + 1):
                        bytecode.append(Inst(InstKind.SWAP))
                        bytecode.append(Inst(InstKind.OVER))
                        if i > 0:
                            bytecode.append(Inst(InstKind.PUSH, arguments=[i]))
                            bytecode.append(Inst(InstKind.ADD))
                        bytecode.append(Inst(InstKind.STORE))
                case OpKind.PUSH_STR:
                    bytecode.append(Inst(InstKind.PUSH_STR, arguments=[op.value]))
                case OpKind.PUSH_VARIABLE:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"

                    # Global variable
                    if variable_name in GLOBAL_VARIABLES:
                        index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
                        bytecode.append(Inst(InstKind.GLOBAL_GET, arguments=[index]))
                        continue

                    # Local variable
                    assert self.function, "Expected function"
                    if variable_name not in self.function.variables:
                        raise NameError(
                            f"Local variable `{variable_name}` does not exist"
                        )

                    index = self.function.variables.index(Variable(variable_name))
                    bytecode.append(Inst(InstKind.LOCAL_GET, arguments=[index]))
                case OpKind.ROT:
                    bytecode.append(Inst(InstKind.ROT))
                case OpKind.STORE:
                    bytecode.append(Inst(InstKind.STORE))
                case OpKind.STRUCT_NEW:
                    struct = op.value
                    assert isinstance(struct, Struct), "Expected struct"

                    # Allocate memory for the struct
                    member_count = len(struct.members)
                    heap_alloc = Inst(InstKind.HEAP_ALLOC, arguments=[member_count])
                    bytecode.append(heap_alloc)

                    # Create the list
                    for i in range(member_count):
                        bytecode.append(Inst(InstKind.SWAP))
                        bytecode.append(Inst(InstKind.OVER))
                        if i > 0:
                            bytecode.append(Inst(InstKind.PUSH, arguments=[i]))
                            bytecode.append(Inst(InstKind.ADD))
                        bytecode.append(Inst(InstKind.STORE))
                case OpKind.SUB:
                    bytecode.append(Inst(InstKind.SUB))
                case OpKind.SWAP:
                    bytecode.append(Inst(InstKind.SWAP))
                case OpKind.WHILE_BREAK:
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                        reverse=True,
                    ):
                        raise SyntaxError("`break` without parent `while`")

                    end_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                    )
                    if not end_label:
                        raise SyntaxError("`break` without matching `done`")

                    bytecode.append(Inst(InstKind.JUMP, arguments=[end_label]))
                case OpKind.WHILE_CONDITION:
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                        reverse=True,
                    ):
                        raise SyntaxError("`do` without parent `while`")

                    end_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                    )
                    if not end_label:
                        raise SyntaxError("`do` without matching `done`")

                    bytecode.append(Inst(InstKind.JUMP_NE, arguments=[end_label]))
                case OpKind.WHILE_CONTINUE:
                    start_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                        reverse=True,
                    )
                    if not start_label:
                        raise SyntaxError("`continue` without parent `while`")

                    bytecode.append(Inst(InstKind.JUMP, arguments=[start_label]))
                case OpKind.WHILE_END:
                    while_label = op_to_label(op)
                    start_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                        reverse=True,
                    )
                    if not start_label:
                        raise SyntaxError("`done` without parent `while`")

                    bytecode.append(Inst(InstKind.JUMP, arguments=[start_label]))
                    bytecode.append(Inst(InstKind.LABEL, arguments=[while_label]))
                case OpKind.WHILE_START:
                    label = op_to_label(op)
                    bytecode.append(Inst(InstKind.LABEL, arguments=[label]))
                case _:
                    assert_never(op.kind)

        if self.function and self.function.variables:
            bytecode.append(
                Inst(InstKind.LOCALS_UNINIT, arguments=[len(self.function.variables)])
            )

        if self.function:
            bytecode.append(Inst(InstKind.FN_RETURN))
        return bytecode

    def find_matching_label(
        self,
        op: Op,
        start_kind: OpKind,
        end_kind: OpKind,
        target_kinds: list[OpKind] | None = None,
        reverse: bool = False,
    ) -> LabelId | None:
        START_KINDS = [OpKind.IF_START, OpKind.WHILE_START]
        END_KINDS = [OpKind.IF_END, OpKind.WHILE_END]
        assert start_kind in START_KINDS, f"Invalid start for block: {start_kind}"
        assert end_kind in END_KINDS, f"Invalid end for block: {end_kind}"

        if not target_kinds:
            target_kinds = [start_kind] if reverse else [end_kind]

        op_index = self.ops.index(op)
        iterable: Iterable[Op] = (
            reversed(self.ops[:op_index]) if reverse else self.ops[op_index + 1 :]
        )

        START_DEPTH = 1
        depth = START_DEPTH
        direction = -1 if reverse else 1
        for other_op in iterable:
            if depth == START_DEPTH and other_op.kind in target_kinds:
                return op_to_label(other_op)

            if other_op.kind in START_KINDS:
                depth += direction
            elif other_op.kind in END_KINDS:
                depth -= direction

            if depth < START_DEPTH:
                return None

        return None
