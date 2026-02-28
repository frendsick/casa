from collections.abc import Iterable
from dataclasses import dataclass, field
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
from casa.error import ErrorKind, raise_error

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
        constants_count=len(compiler.constants_table),
    )


@dataclass
class Compiler:
    ops: list[Op]
    function: Function | None = None
    locals_count: int = 0
    string_table: list[str] = field(default_factory=list)
    constants_table: list[str] = field(default_factory=list)

    def __post_init__(self):
        self._current_loc: Location | None = None

    @staticmethod
    def _intern(table: list[str], value: str) -> int:
        """Add a value to a table if not present and return its index."""
        if value in table:
            return table.index(value)
        table.append(value)
        return len(table) - 1

    def intern_string(self, value: str) -> int:
        """Add a string to the string table and return its index."""
        return self._intern(self.string_table, value)

    def intern_constant(self, name: str) -> int:
        """Add a constant name to the constants table and return its index."""
        return self._intern(self.constants_table, name)

    def inst(self, kind: InstKind, args: list | None = None) -> Inst:
        """Create an Inst with the current source location."""
        return Inst(kind, args=args or [], location=self._current_loc)

    def _resolve_variable(
        self, variable_name: str
    ) -> tuple[InstKind, InstKind, int]:
        """Resolve a variable to its get/set instructions and index."""
        if self.function and variable_name in self.function.variables:
            index = self.function.variables.index(Variable(variable_name))
            return InstKind.LOCAL_GET, InstKind.LOCAL_SET, index
        if variable_name in GLOBAL_VARIABLES:
            index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
            return InstKind.GLOBAL_GET, InstKind.GLOBAL_SET, index
        raise AssertionError(
            f"Variable `{variable_name}` is not defined"
        )

    def _compile_function(self, function: Function, op: Op) -> None:
        """Compile a function if not already compiled."""
        if function.bytecode is not None:
            return
        # Check if the function assigns a non-parameter
        # variable that shadows a global variable
        param_names = set()
        if function.signature:
            param_names = {p.name for p in function.signature.parameters if p.name}
        for var in function.variables:
            if var in GLOBAL_VARIABLES and var.name not in param_names:
                raise_error(
                    ErrorKind.UNDEFINED_NAME,
                    f"Function `{function.name}` assigns a global variable `{var.name}` before it is initialized within the global scope",
                    op.location,
                )
        if self.function != function:
            function.bytecode = []
            fn_compiler = Compiler(
                function.ops,
                function,
                string_table=self.string_table,
                constants_table=self.constants_table,
            )
            function.bytecode = fn_compiler.compile()

    def compile(self) -> Bytecode:
        assert len(InstKind) == 66, "Exhaustive handling for `InstructionKind"
        assert len(OpKind) == 79, "Exhaustive handling for `OpKind`"

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
                    get_kind, set_kind, index = self._resolve_variable(
                        variable_name
                    )
                    bytecode.append(self.inst(get_kind, args=[index]))
                    bytecode.append(self.inst(InstKind.SWAP))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(set_kind, args=[index]))
                case OpKind.ASSIGN_INCREMENT:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"
                    get_kind, set_kind, index = self._resolve_variable(
                        variable_name
                    )
                    bytecode.append(self.inst(get_kind, args=[index]))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(set_kind, args=[index]))
                case OpKind.ASSIGN_VARIABLE:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"
                    _, set_kind, index = self._resolve_variable(
                        variable_name
                    )
                    bytecode.append(self.inst(set_kind, args=[index]))
                case OpKind.DIV:
                    bytecode.append(self.inst(InstKind.DIV))
                case OpKind.DROP:
                    bytecode.append(self.inst(InstKind.DROP))
                case OpKind.DUP:
                    bytecode.append(self.inst(InstKind.DUP))
                case OpKind.EQ:
                    bytecode.append(self.inst(InstKind.EQ))
                case OpKind.FSTRING_CONCAT:
                    count = op.value
                    assert isinstance(count, int)
                    bytecode.append(self.inst(InstKind.FSTRING_CONCAT, args=[count]))
                case OpKind.FN_CALL:
                    function_name = op.value
                    function = GLOBAL_FUNCTIONS.get(function_name)
                    assert isinstance(function, Function), "Expected function"

                    self._compile_function(function, op)

                    bytecode.append(self.inst(InstKind.FN_CALL, args=[function_name]))
                case OpKind.FN_EXEC:
                    bytecode.append(self.inst(InstKind.FN_EXEC))
                case OpKind.FN_PUSH:
                    function_name = op.value
                    lambda_function = GLOBAL_FUNCTIONS.get(function_name)
                    if not lambda_function:
                        raise_error(
                            ErrorKind.UNDEFINED_NAME,
                            f"Function `{function_name}` is not defined",
                            op.location,
                        )

                    self._compile_function(lambda_function, op)

                    # Setup captures (local variables shadow globals)
                    for index, capture in enumerate(lambda_function.captures):
                        if self.function and capture in self.function.variables:
                            index = self.function.variables.index(capture)
                            bytecode.append(self.inst(InstKind.LOCAL_GET, args=[index]))
                        elif capture in GLOBAL_VARIABLES:
                            index = list(GLOBAL_VARIABLES.values()).index(capture)
                            bytecode.append(
                                self.inst(InstKind.GLOBAL_GET, args=[index])
                            )
                        else:
                            raise AssertionError("Captured variable should exist")

                        constant_index = self.intern_constant(
                            f"{lambda_function.name}_{capture.name}"
                        )
                        bytecode.append(
                            self.inst(
                                InstKind.CONSTANT_STORE,
                                args=[constant_index],
                            )
                        )

                    # Push function address
                    bytecode.append(self.inst(InstKind.FN_PUSH, args=[function_name]))

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
                    self._require_matching_label(
                        op, OpKind.IF_START, OpKind.IF_END,
                        "`then` without matching `if`",
                        reverse=True,
                    )
                    end_label = self._require_matching_label(
                        op, OpKind.IF_START, OpKind.IF_END,
                        "`then` without matching `fi`",
                        target_kinds=[
                            OpKind.IF_ELIF, OpKind.IF_ELSE, OpKind.IF_END
                        ],
                    )
                    bytecode.append(self.inst(InstKind.JUMP_NE, args=[end_label]))
                case OpKind.IF_ELIF:
                    self._require_matching_label(
                        op, OpKind.IF_START, OpKind.IF_END,
                        "`elif` without parent `if`",
                        reverse=True,
                    )
                    # Elif should not be after an else
                    if self.find_matching_label(
                        op=op,
                        start_kind=OpKind.IF_START,
                        end_kind=OpKind.IF_END,
                        target_kinds=[OpKind.IF_ELSE],
                        reverse=True,
                    ):
                        raise_error(
                            ErrorKind.UNMATCHED_BLOCK,
                            "`elif` after `else`",
                            op.location,
                        )

                    elif_label = op_to_label(op)
                    end_label = self._require_matching_label(
                        op, OpKind.IF_START, OpKind.IF_END,
                        "`elif` without matching `fi`",
                    )
                    bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
                    bytecode.append(self.inst(InstKind.LABEL, args=[elif_label]))
                case OpKind.IF_ELSE:
                    self._require_matching_label(
                        op, OpKind.IF_START, OpKind.IF_END,
                        "`else` without parent `if`",
                        reverse=True,
                    )
                    else_label = op_to_label(op)
                    end_label = self._require_matching_label(
                        op, OpKind.IF_START, OpKind.IF_END,
                        "`else` without matching `fi`",
                    )
                    bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
                    bytecode.append(self.inst(InstKind.LABEL, args=[else_label]))
                case OpKind.IF_END:
                    self._require_matching_label(
                        op, OpKind.IF_START, OpKind.IF_END,
                        "`fi` without parent `if`",
                        reverse=True,
                    )
                    label = op_to_label(op)
                    bytecode.append(self.inst(InstKind.LABEL, args=[label]))
                case OpKind.IF_START:
                    self._require_matching_label(
                        op, OpKind.IF_START, OpKind.IF_END,
                        "`if` without matching `fi`",
                    )
                case OpKind.INCLUDE_FILE:
                    pass
                case OpKind.LE:
                    bytecode.append(self.inst(InstKind.LE))
                case OpKind.LOAD8:
                    bytecode.append(self.inst(InstKind.LOAD8))
                case OpKind.LOAD16:
                    bytecode.append(self.inst(InstKind.LOAD16))
                case OpKind.LOAD32:
                    bytecode.append(self.inst(InstKind.LOAD32))
                case OpKind.LOAD64:
                    bytecode.append(self.inst(InstKind.LOAD64))
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
                    assert (
                        False
                    ), "PRINT should be resolved to a PRINT variant by the type checker"
                case OpKind.PRINT_BOOL:
                    bytecode.append(self.inst(InstKind.PRINT_BOOL))
                case OpKind.PRINT_CHAR:
                    bytecode.append(self.inst(InstKind.PRINT_CHAR))
                case OpKind.PRINT_CSTR:
                    bytecode.append(self.inst(InstKind.PRINT_CSTR))
                case OpKind.PRINT_INT:
                    bytecode.append(self.inst(InstKind.PRINT_INT))
                case OpKind.PRINT_STR:
                    bytecode.append(self.inst(InstKind.PRINT_STR))
                case OpKind.PUSH_ARRAY:
                    assert isinstance(op.value, list), "Expected `list`"
                    list_len = len(op.value)

                    # Push array items in the reverse order
                    reversed_items: list[Op] = list(reversed(op.value))
                    list_compiler = Compiler(
                        reversed_items,
                        string_table=self.string_table,
                        constants_table=self.constants_table,
                    )
                    list_bytecode = list_compiler.compile()
                    bytecode += list_bytecode

                    # Allocate memory: header (data_ptr + length) + elements
                    local_list = self.locals_count
                    bytecode.append(self.inst(InstKind.PUSH, args=[(list_len + 2) * 8]))
                    bytecode.append(self.inst(InstKind.HEAP_ALLOC))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_list]))
                    self.locals_count += 1

                    # Store data_ptr (allocation + 16) at offset 0
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_list]))
                    bytecode.append(self.inst(InstKind.PUSH, args=[16]))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_list]))
                    bytecode.append(self.inst(InstKind.STORE64))

                    # Store length at offset 8
                    bytecode.append(self.inst(InstKind.PUSH, args=[list_len]))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_list]))
                    bytecode.append(self.inst(InstKind.PUSH, args=[8]))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(InstKind.STORE64))

                    # Store elements starting at offset 16
                    local_index = self.locals_count
                    bytecode.append(self.inst(InstKind.PUSH, args=[16]))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_index]))
                    self.locals_count += 1

                    start_label = new_label()
                    end_label = new_label()

                    # while index limit > do
                    bytecode.append(self.inst(InstKind.LABEL, args=[start_label]))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_index]))
                    bytecode.append(self.inst(InstKind.PUSH, args=[(list_len + 2) * 8]))
                    bytecode.append(self.inst(InstKind.GE))
                    bytecode.append(self.inst(InstKind.JUMP_NE, args=[end_label]))

                    # list index + store64
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_list]))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_index]))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(InstKind.STORE64))

                    # 8 += index
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_index]))
                    bytecode.append(self.inst(InstKind.PUSH, args=[8]))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_index]))

                    # done
                    bytecode.append(self.inst(InstKind.JUMP, args=[start_label]))
                    bytecode.append(self.inst(InstKind.LABEL, args=[end_label]))

                    # Push array to the stack
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_list]))
                case OpKind.PUSH_BOOL:
                    bytecode.append(self.inst(InstKind.PUSH, args=[int(op.value)]))
                case OpKind.PUSH_CHAR:
                    bytecode.append(self.inst(InstKind.PUSH_CHAR, args=[op.value]))
                case OpKind.PUSH_CAPTURE:
                    capture_name = op.value
                    assert isinstance(capture_name, str), "Valid capture name"

                    function_name = (
                        self.function.name if self.function else GLOBAL_SCOPE_LABEL
                    )
                    constant_index = self.intern_constant(
                        f"{function_name}_{capture_name}"
                    )
                    bytecode.append(
                        self.inst(
                            InstKind.CONSTANT_LOAD,
                            args=[constant_index],
                        )
                    )
                case OpKind.PUSH_INT:
                    bytecode.append(self.inst(InstKind.PUSH, args=[op.value]))
                case OpKind.PUSH_NONE:
                    # Allocate 16 bytes, store tag 0 at byte offset 0
                    bytecode.append(self.inst(InstKind.PUSH, args=[16]))
                    bytecode.append(self.inst(InstKind.HEAP_ALLOC))
                    bytecode.append(self.inst(InstKind.DUP))
                    bytecode.append(self.inst(InstKind.PUSH, args=[0]))
                    bytecode.append(self.inst(InstKind.SWAP))
                    bytecode.append(self.inst(InstKind.STORE64))
                case OpKind.PUSH_STR:
                    string_index = self.intern_string(op.value)
                    bytecode.append(self.inst(InstKind.PUSH_STR, args=[string_index]))
                case OpKind.PUSH_VARIABLE:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"
                    get_kind, _, index = self._resolve_variable(
                        variable_name
                    )
                    bytecode.append(self.inst(get_kind, args=[index]))
                case OpKind.ROT:
                    bytecode.append(self.inst(InstKind.ROT))
                case OpKind.SHL:
                    bytecode.append(self.inst(InstKind.SHL))
                case OpKind.SHR:
                    bytecode.append(self.inst(InstKind.SHR))
                case OpKind.BIT_AND:
                    bytecode.append(self.inst(InstKind.BIT_AND))
                case OpKind.BIT_OR:
                    bytecode.append(self.inst(InstKind.BIT_OR))
                case OpKind.BIT_XOR:
                    bytecode.append(self.inst(InstKind.BIT_XOR))
                case OpKind.BIT_NOT:
                    bytecode.append(self.inst(InstKind.BIT_NOT))
                case OpKind.SOME:
                    # Stack: [value]
                    # Allocate 16 bytes, store tag=1 at byte 0, value at byte 8
                    local_ptr = self.locals_count
                    self.locals_count += 1
                    bytecode.append(self.inst(InstKind.PUSH, args=[16]))
                    bytecode.append(self.inst(InstKind.HEAP_ALLOC))
                    bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_ptr]))
                    # Store value at byte offset 8
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_ptr]))
                    bytecode.append(self.inst(InstKind.PUSH, args=[8]))
                    bytecode.append(self.inst(InstKind.ADD))
                    bytecode.append(self.inst(InstKind.STORE64))
                    # Store tag=1 at byte offset 0
                    bytecode.append(self.inst(InstKind.PUSH, args=[1]))
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_ptr]))
                    bytecode.append(self.inst(InstKind.STORE64))
                    # Push ptr
                    bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_ptr]))
                case OpKind.STORE8:
                    bytecode.append(self.inst(InstKind.STORE8))
                case OpKind.STORE16:
                    bytecode.append(self.inst(InstKind.STORE16))
                case OpKind.STORE32:
                    bytecode.append(self.inst(InstKind.STORE32))
                case OpKind.STORE64:
                    bytecode.append(self.inst(InstKind.STORE64))
                case OpKind.STRUCT_NEW:
                    struct = op.value
                    assert isinstance(struct, Struct), "Expected struct"

                    # Allocate memory for the struct (byte-based)
                    member_count = len(struct.members)
                    bytecode.append(self.inst(InstKind.PUSH, args=[member_count * 8]))
                    bytecode.append(self.inst(InstKind.HEAP_ALLOC))

                    # Store each member
                    for i in range(member_count):
                        bytecode.append(self.inst(InstKind.SWAP))
                        bytecode.append(self.inst(InstKind.OVER))
                        if i > 0:
                            bytecode.append(self.inst(InstKind.PUSH, args=[i * 8]))
                            bytecode.append(self.inst(InstKind.ADD))
                        bytecode.append(self.inst(InstKind.STORE64))
                case OpKind.SUB:
                    bytecode.append(self.inst(InstKind.SUB))
                case OpKind.SWAP:
                    bytecode.append(self.inst(InstKind.SWAP))
                case OpKind.TYPE_CAST:
                    pass
                case OpKind.WHILE_BREAK:
                    self._require_matching_label(
                        op, OpKind.WHILE_START, OpKind.WHILE_END,
                        "`break` without parent `while`",
                        reverse=True,
                    )
                    end_label = self._require_matching_label(
                        op, OpKind.WHILE_START, OpKind.WHILE_END,
                        "`break` without matching `done`",
                    )
                    bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
                case OpKind.WHILE_CONDITION:
                    self._require_matching_label(
                        op, OpKind.WHILE_START, OpKind.WHILE_END,
                        "`do` without parent `while`",
                        reverse=True,
                    )
                    end_label = self._require_matching_label(
                        op, OpKind.WHILE_START, OpKind.WHILE_END,
                        "`do` without matching `done`",
                    )
                    bytecode.append(self.inst(InstKind.JUMP_NE, args=[end_label]))
                case OpKind.WHILE_CONTINUE:
                    continue_target = self._require_matching_label(
                        op, OpKind.WHILE_START, OpKind.WHILE_END,
                        "`continue` without parent `while`",
                        reverse=True,
                    )
                    bytecode.append(self.inst(InstKind.JUMP, args=[continue_target]))
                case OpKind.WHILE_END:
                    while_label = op_to_label(op)
                    loop_start = self._require_matching_label(
                        op, OpKind.WHILE_START, OpKind.WHILE_END,
                        "`done` without parent `while`",
                        reverse=True,
                    )
                    bytecode.append(self.inst(InstKind.JUMP, args=[loop_start]))
                    bytecode.append(self.inst(InstKind.LABEL, args=[while_label]))
                case OpKind.WHILE_START:
                    label = op_to_label(op)
                    bytecode.append(self.inst(InstKind.LABEL, args=[label]))
                case OpKind.SYSCALL0:
                    bytecode.append(self.inst(InstKind.SYSCALL0))
                case OpKind.SYSCALL1:
                    bytecode.append(self.inst(InstKind.SYSCALL1))
                case OpKind.SYSCALL2:
                    bytecode.append(self.inst(InstKind.SYSCALL2))
                case OpKind.SYSCALL3:
                    bytecode.append(self.inst(InstKind.SYSCALL3))
                case OpKind.SYSCALL4:
                    bytecode.append(self.inst(InstKind.SYSCALL4))
                case OpKind.SYSCALL5:
                    bytecode.append(self.inst(InstKind.SYSCALL5))
                case OpKind.SYSCALL6:
                    bytecode.append(self.inst(InstKind.SYSCALL6))
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
            bytecode.append(self.inst(InstKind.FN_RETURN))

        return bytecode

    def _require_matching_label(
        self,
        op: Op,
        start_kind: OpKind,
        end_kind: OpKind,
        message: str,
        **kwargs,
    ) -> LabelId:
        """Find a matching label or raise an error."""
        label = self.find_matching_label(
            op=op, start_kind=start_kind, end_kind=end_kind, **kwargs
        )
        if label is None:
            raise_error(ErrorKind.UNMATCHED_BLOCK, message, op.location)
        return label

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
