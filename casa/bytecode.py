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
    Location,
    Op,
    OpKind,
    Program,
    Struct,
    Variable,
)


_label_counter = 0


def new_label() -> LabelId:
    global _label_counter
    _label_counter += 1
    return _label_counter


def reset_labels():
    global _label_counter
    _label_counter = 0


# Stable mapping from Op identity to label, assigned on first access
_op_label_map: dict[int, LabelId] = {}


def op_to_label(op: Op) -> LabelId:
    op_id = id(op)
    if op_id not in _op_label_map:
        _op_label_map[op_id] = new_label()
    return _op_label_map[op_id]


def compile_bytecode(ops: list[Op]) -> Program:
    reset_labels()
    _op_label_map.clear()

    bytecode: Bytecode = []
    compiler = Compiler(ops)

    if len(GLOBAL_VARIABLES) > 0:
        bytecode.append(Inst(InstKind.GLOBALS_INIT, args=[len(GLOBAL_VARIABLES)]))

    bytecode += compiler.compile()

    return Program(
        bytecode=bytecode,
        functions={
            name: fn.bytecode
            for name, fn in GLOBAL_FUNCTIONS.items()
            if fn.bytecode is not None
        },
        strings=compiler.string_table,
        globals_count=len(GLOBAL_VARIABLES),
    )


@dataclass
class Compiler:
    ops: list[Op]
    function: Function | None = None
    locals_count: int = 0
    string_table: list[str] | None = None

    def __post_init__(self):
        if self.string_table is None:
            self.string_table = []
        self._current_loc: Location | None = None

    def intern_string(self, s: str) -> int:
        """Add a string to the string table and return its index."""
        if s in self.string_table:
            return self.string_table.index(s)
        self.string_table.append(s)
        return len(self.string_table) - 1

    def inst(self, kind: InstKind, args: list | None = None) -> Inst:
        """Create an Inst with the current source location."""
        return Inst(kind, args=args or [], location=self._current_loc)

    def compile(self) -> Bytecode:
        assert len(InstKind) == 42, "Exhaustive handling for `InstructionKind"
        assert len(OpKind) == 53, "Exhaustive handling for `OpKind`"

        cursor = Cursor(sequence=self.ops)
        bytecode: list[Inst] = []
        if self.function:
            self.locals_count = len(self.function.variables)

        # Function label
        fn_label = new_label()
        bytecode.append(Inst(InstKind.LABEL, args=[fn_label]))

        while op := cursor.pop():
            self._current_loc = op.location
            match op.kind:
                case OpKind.ADD:
                    bytecode.append(self.inst(InstKind.ADD))
                case OpKind.AND:
                    bytecode.append(self.inst(InstKind.AND))
                case OpKind.ASSIGN_DECREMENT:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"

                    if variable_name in GLOBAL_VARIABLES:
                        index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
                        bytecode.append(self.inst(InstKind.GLOBAL_GET, args=[index]))
                        bytecode.append(self.inst(InstKind.SWAP))
                        bytecode.append(self.inst(InstKind.ADD))
                        bytecode.append(self.inst(InstKind.GLOBAL_SET, args=[index]))
                    elif self.function and variable_name in self.function.variables:
                        index = self.function.variables.index(Variable(variable_name))
                        bytecode.append(self.inst(InstKind.LOCAL_GET, args=[index]))
                        bytecode.append(self.inst(InstKind.SWAP))
                        bytecode.append(self.inst(InstKind.ADD))
                        bytecode.append(self.inst(InstKind.LOCAL_SET, args=[index]))
                    else:
                        raise AssertionError(
                            f"Variable `{variable_name}` is not defined"
                        )
                case OpKind.ASSIGN_INCREMENT:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"

                    if variable_name in GLOBAL_VARIABLES:
                        index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
                        bytecode.append(self.inst(InstKind.GLOBAL_GET, args=[index]))
                        bytecode.append(self.inst(InstKind.ADD))
                        bytecode.append(self.inst(InstKind.GLOBAL_SET, args=[index]))
                    elif self.function and variable_name in self.function.variables:
                        index = self.function.variables.index(Variable(variable_name))
                        bytecode.append(self.inst(InstKind.LOCAL_GET, args=[index]))
                        bytecode.append(self.inst(InstKind.ADD))
                        bytecode.append(self.inst(InstKind.LOCAL_SET, args=[index]))
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
                        bytecode.append(self.inst(InstKind.GLOBAL_SET, args=[index]))
                        continue

                    # Local variable
                    assert self.function, "Function exists"
                    assert variable_name in self.function.variables, "Variable exists"
                    index = self.function.variables.index(Variable(variable_name))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[index]))
                case OpKind.DIV:
                    bytecode.append(self.inst(InstKind.DIV))
                case OpKind.DROP:
                    bytecode.append(self.inst(InstKind.DROP))
                case OpKind.DUP:
                    bytecode.append(self.inst(InstKind.DUP))
                case OpKind.EQ:
                    bytecode.append(self.inst(InstKind.EQ))
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

                        if self.function != function:
                            fn_compiler = Compiler(
                                function.ops, function,
                                string_table=self.string_table,
                            )
                            function.bytecode = fn_compiler.compile()

                    bytecode.append(self.inst(InstKind.FN_CALL, args=[function_name]))
                case OpKind.FN_EXEC:
                    bytecode.append(self.inst(InstKind.FN_EXEC))
                case OpKind.FN_PUSH:
                    function_name = op.value
                    lambda_function = GLOBAL_FUNCTIONS.get(function_name)
                    if not lambda_function:
                        raise NameError(f"Function `{function_name}` is not defined")

                    # Compile the lambda function
                    fn_compiler = Compiler(
                        lambda_function.ops, lambda_function,
                        string_table=self.string_table,
                    )
                    lambda_function.bytecode = fn_compiler.compile()

                    # Setup captures
                    for index, capture in enumerate(lambda_function.captures):
                        if capture in GLOBAL_VARIABLES:
                            index = list(GLOBAL_VARIABLES.values()).index(capture)
                            bytecode.append(self.inst(InstKind.GLOBAL_GET, args=[index]))
                        elif self.function and capture in self.function.variables:
                            index = self.function.variables.index(capture)
                            bytecode.append(self.inst(InstKind.LOCAL_GET, args=[index]))
                        else:
                            raise AssertionError("Captured variable should exist")

                        bytecode.append(
                            self.inst(
                                InstKind.CONSTANT_STORE,
                                args=[f"{lambda_function.name}_{capture.name}"],
                            )
                        )

                    # Push function pointer
                    index = list(GLOBAL_FUNCTIONS).index(function_name)
                    bytecode.append(self.inst(InstKind.PUSH, args=[index]))

                case OpKind.FN_RETURN:
                    bytecode.append(self.inst(InstKind.FN_RETURN))
                case OpKind.GE:
                    bytecode.append(self.inst(InstKind.GE))
                case OpKind.GT:
                    bytecode.append(self.inst(InstKind.GT))
                case OpKind.HEAP_ALLOC:
                    bytecode.append(self.inst(InstKind.HEAP_ALLOC))
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

                    bytecode.append(self.inst(InstKind.JUMP_NE, args=[end_label]))
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

                    bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
                    bytecode.append(self.inst(InstKind.LABEL, args=[elif_label]))
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

                    bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
                    bytecode.append(self.inst(InstKind.LABEL, args=[else_label]))
                case OpKind.IF_END:
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                        reverse=True,
                    ):
                        raise SyntaxError("`fi` without parent `if`")
                    label = op_to_label(op)
                    bytecode.append(self.inst(InstKind.LABEL, args=[label]))
                case OpKind.IF_START:
                    if not self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                    ):
                        raise SyntaxError("`if` without matching `fi`")
                case OpKind.INCLUDE_FILE:
                    pass
                case OpKind.LE:
                    bytecode.append(self.inst(InstKind.LE))
                case OpKind.LOAD:
                    bytecode.append(self.inst(InstKind.LOAD))
                case OpKind.LT:
                    bytecode.append(self.inst(InstKind.LT))
                case OpKind.METHOD_CALL:
                    raise AssertionError(
                        "Method call should be transpiled to function call during type checking"
                    )
                case OpKind.MOD:
                    bytecode.append(self.inst(InstKind.MOD))
                case OpKind.MUL:
                    bytecode.append(self.inst(InstKind.MUL))
                case OpKind.NE:
                    bytecode.append(self.inst(InstKind.NE))
                case OpKind.NOT:
                    bytecode.append(self.inst(InstKind.NOT))
                case OpKind.OR:
                    bytecode.append(self.inst(InstKind.OR))
                case OpKind.OVER:
                    bytecode.append(self.inst(InstKind.OVER))
                case OpKind.PRINT:
                    bytecode.append(self.inst(InstKind.PRINT))
                case OpKind.PUSH_ARRAY:
                    assert isinstance(op.value, list), "Expected `list`"
                    list_len = len(op.value)

                    # Push array items in the reverse order
                    reversed_items: list[Op] = list(reversed(op.value))
                    list_compiler = Compiler(
                        reversed_items,
                        string_table=self.string_table,
                    )
                    list_bytecode = list_compiler.compile()
                    bytecode += list_bytecode

                    # Allocate memory for the array
                    local_list = self.locals_count
                    bytecode.append(self.inst(InstKind.PUSH, args=[list_len + 1]))
                    bytecode.append(self.inst(InstKind.HEAP_ALLOC))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_list]))
                    self.locals_count += 1

                    # First item of the array is its length
                    local_len = self.locals_count
                    bytecode.append(self.inst(InstKind.PUSH, args=[list_len]))
                    bytecode.append(self.inst(InstKind.DUP))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_len]))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_list]))
                    bytecode.append(self.inst(InstKind.STORE))
                    self.locals_count += 1

                    # Create the array
                    local_index = self.locals_count
                    bytecode.append(self.inst(InstKind.PUSH, args=[1]))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_index]))
                    self.locals_count += 1

                    start_label = new_label()
                    end_label = new_label()

                    # while index len > do
                    bytecode.append(self.inst(InstKind.LABEL, args=[start_label]))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_index]))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_len]))
                    bytecode.append(self.inst(InstKind.GE))
                    bytecode.append(self.inst(InstKind.JUMP_NE, args=[end_label]))

                    # list index + store
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_list]))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_index]))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(InstKind.STORE))

                    # 1 += index
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_index]))
                    bytecode.append(self.inst(InstKind.PUSH, args=[1]))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_index]))

                    # done
                    bytecode.append(self.inst(InstKind.JUMP, args=[start_label]))
                    bytecode.append(self.inst(InstKind.LABEL, args=[end_label]))

                    # Push array to the stack
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_list]))
                case OpKind.PUSH_BOOL:
                    bytecode.append(self.inst(InstKind.PUSH, args=[int(op.value)]))
                case OpKind.PUSH_CAPTURE:
                    capture_name = op.value
                    assert isinstance(capture_name, str), "Valid capture name"

                    function_name = (
                        self.function.name if self.function else GLOBAL_SCOPE_LABEL
                    )
                    bytecode.append(
                        self.inst(
                            InstKind.CONSTANT_LOAD,
                            args=[f"{function_name}_{capture_name}"],
                        )
                    )
                case OpKind.PUSH_INT:
                    bytecode.append(self.inst(InstKind.PUSH, args=[op.value]))
                case OpKind.PUSH_STR:
                    string_index = self.intern_string(op.value)
                    bytecode.append(self.inst(InstKind.PUSH_STR, args=[string_index]))
                case OpKind.PUSH_VARIABLE:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"

                    # Global variable
                    if variable_name in GLOBAL_VARIABLES:
                        index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
                        bytecode.append(self.inst(InstKind.GLOBAL_GET, args=[index]))
                        continue

                    # Local variable
                    assert self.function, "Expected function"
                    if variable_name not in self.function.variables:
                        raise NameError(
                            f"Local variable `{variable_name}` does not exist"
                        )

                    index = self.function.variables.index(Variable(variable_name))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[index]))
                case OpKind.ROT:
                    bytecode.append(self.inst(InstKind.ROT))
                case OpKind.SHL:
                    bytecode.append(self.inst(InstKind.SHL))
                case OpKind.SHR:
                    bytecode.append(self.inst(InstKind.SHR))
                case OpKind.STORE:
                    bytecode.append(self.inst(InstKind.STORE))
                case OpKind.STRUCT_NEW:
                    struct = op.value
                    assert isinstance(struct, Struct), "Expected struct"

                    # Allocate memory for the struct
                    member_count = len(struct.members)
                    bytecode.append(self.inst(InstKind.PUSH, args=[member_count]))
                    bytecode.append(self.inst(InstKind.HEAP_ALLOC))

                    # Create the list
                    for i in range(member_count):
                        bytecode.append(self.inst(InstKind.SWAP))
                        bytecode.append(self.inst(InstKind.OVER))
                        if i > 0:
                            bytecode.append(self.inst(InstKind.PUSH, args=[i]))
                            bytecode.append(self.inst(InstKind.ADD))
                        bytecode.append(self.inst(InstKind.STORE))
                case OpKind.SUB:
                    bytecode.append(self.inst(InstKind.SUB))
                case OpKind.SWAP:
                    bytecode.append(self.inst(InstKind.SWAP))
                case OpKind.TYPE_CAST:
                    pass
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

                    bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
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

                    bytecode.append(self.inst(InstKind.JUMP_NE, args=[end_label]))
                case OpKind.WHILE_CONTINUE:
                    start_label = self.find_matching_label(
                        op=op,
                        start_kind=OpKind.WHILE_START,
                        end_kind=OpKind.WHILE_END,
                        reverse=True,
                    )
                    if not start_label:
                        raise SyntaxError("`continue` without parent `while`")

                    bytecode.append(self.inst(InstKind.JUMP, args=[start_label]))
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

                    bytecode.append(self.inst(InstKind.JUMP, args=[start_label]))
                    bytecode.append(self.inst(InstKind.LABEL, args=[while_label]))
                case OpKind.WHILE_START:
                    label = op_to_label(op)
                    bytecode.append(self.inst(InstKind.LABEL, args=[label]))
                case _:
                    assert_never(op.kind)

        if self.locals_count > 0:
            bytecode.insert(0, Inst(InstKind.LOCALS_INIT, args=[self.locals_count]))
            locals_uninit = Inst(InstKind.LOCALS_UNINIT, args=[self.locals_count])
            bytecode.append(locals_uninit)

            i = 0
            while i < len(bytecode):
                instruction = bytecode[i]
                if instruction.kind == InstKind.FN_RETURN:
                    bytecode.insert(i, locals_uninit)
                    i += 1
                i += 1

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
            if depth <= START_DEPTH and other_op.kind in target_kinds:
                return op_to_label(other_op)

            if other_op.kind in START_KINDS:
                depth += direction
            elif other_op.kind in END_KINDS:
                depth -= direction

            if depth < START_DEPTH and (
                (op.kind in START_KINDS and not reverse)
                or (op.kind in END_KINDS and reverse)
            ):
                return None

        return None
