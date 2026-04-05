"""Op-to-Inst bytecode lowering for the Casa compiler."""

from collections.abc import Iterable
from dataclasses import dataclass, field

from casa.common import (
    GLOBAL_ENUMS,
    GLOBAL_FUNCTIONS,
    GLOBAL_SCOPE_LABEL,
    GLOBAL_STRUCTS,
    GLOBAL_VARIABLES,
    Bytecode,
    CasaEnum,
    Cursor,
    EnumVariant,
    Function,
    Inst,
    InstKind,
    LabelId,
    LiteralPattern,
    Location,
    Op,
    OpKind,
    Program,
    Struct,
    StructLiteral,
    StructPattern,
    Variable,
)
from casa.error import ErrorKind, raise_error

DIRECT_OP_TO_INST: dict[OpKind, InstKind] = {
    OpKind.ADD: InstKind.ADD,
    OpKind.AND: InstKind.AND,
    OpKind.BIT_AND: InstKind.BIT_AND,
    OpKind.BIT_NOT: InstKind.BIT_NOT,
    OpKind.BIT_OR: InstKind.BIT_OR,
    OpKind.BIT_XOR: InstKind.BIT_XOR,
    OpKind.DIV: InstKind.DIV,
    OpKind.DROP: InstKind.DROP,
    OpKind.DUP: InstKind.DUP,
    OpKind.EQ: InstKind.EQ,
    OpKind.FN_EXEC: InstKind.FN_EXEC,
    OpKind.FN_RETURN: InstKind.FN_RETURN,
    OpKind.GE: InstKind.GE,
    OpKind.GT: InstKind.GT,
    OpKind.HEAP_ALLOC: InstKind.HEAP_ALLOC,
    OpKind.LE: InstKind.LE,
    OpKind.LOAD8: InstKind.LOAD8,
    OpKind.LOAD16: InstKind.LOAD16,
    OpKind.LOAD32: InstKind.LOAD32,
    OpKind.LOAD64: InstKind.LOAD64,
    OpKind.LT: InstKind.LT,
    OpKind.MOD: InstKind.MOD,
    OpKind.MUL: InstKind.MUL,
    OpKind.NE: InstKind.NE,
    OpKind.NOT: InstKind.NOT,
    OpKind.OR: InstKind.OR,
    OpKind.OVER: InstKind.OVER,
    OpKind.PRINT_BOOL: InstKind.PRINT_BOOL,
    OpKind.PRINT_CHAR: InstKind.PRINT_CHAR,
    OpKind.PRINT_CSTR: InstKind.PRINT_CSTR,
    OpKind.PRINT_INT: InstKind.PRINT_INT,
    OpKind.PRINT_STR: InstKind.PRINT_STR,
    OpKind.ROT: InstKind.ROT,
    OpKind.SHL: InstKind.SHL,
    OpKind.SHR: InstKind.SHR,
    OpKind.STORE8: InstKind.STORE8,
    OpKind.STORE16: InstKind.STORE16,
    OpKind.STORE32: InstKind.STORE32,
    OpKind.STORE64: InstKind.STORE64,
    OpKind.SUB: InstKind.SUB,
    OpKind.SWAP: InstKind.SWAP,
    OpKind.SYSCALL0: InstKind.SYSCALL0,
    OpKind.SYSCALL1: InstKind.SYSCALL1,
    OpKind.SYSCALL2: InstKind.SYSCALL2,
    OpKind.SYSCALL3: InstKind.SYSCALL3,
    OpKind.SYSCALL4: InstKind.SYSCALL4,
    OpKind.SYSCALL5: InstKind.SYSCALL5,
    OpKind.SYSCALL6: InstKind.SYSCALL6,
    OpKind.ARGC: InstKind.ARGC,
    OpKind.ARGV: InstKind.ARGV,
}

_label_counter = [0]


def new_label() -> LabelId:
    """Allocate and return a fresh label ID."""
    _label_counter[0] += 1
    return _label_counter[0]


def reset_labels():
    """Reset the label counter to zero."""
    _label_counter[0] = 0


# Stable mapping from Op identity to label, assigned on first access
_op_label_map: dict[int, LabelId] = {}


def op_to_label(op: Op) -> LabelId:
    """Return a stable label for an op, allocating one on first access."""
    op_id = id(op)
    if op_id not in _op_label_map:
        _op_label_map[op_id] = new_label()
    return _op_label_map[op_id]


def compile_bytecode(ops: list[Op]) -> Program:
    """Compile a list of ops into a bytecode program."""
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
    """Lowers a list of ops into bytecode instructions."""

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

    def _resolve_variable(self, variable_name: str) -> tuple[InstKind, InstKind, int]:
        """Resolve a variable to its get/set instructions and index."""
        if self.function and variable_name in self.function.variables:
            index = self.function.variables.index(Variable(variable_name))
            return InstKind.LOCAL_GET, InstKind.LOCAL_SET, index
        if variable_name in GLOBAL_VARIABLES:
            index = list(GLOBAL_VARIABLES.keys()).index(variable_name)
            return InstKind.GLOBAL_GET, InstKind.GLOBAL_SET, index
        raise AssertionError(f"Variable `{variable_name}` is not defined")

    def _compile_assignment(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile ASSIGN_VARIABLE, ASSIGN_INCREMENT, ASSIGN_DECREMENT ops."""
        variable_name = op.value
        assert isinstance(variable_name, str), "Valid variable name"
        get_kind, set_kind, index = self._resolve_variable(variable_name)
        match op.kind:
            case OpKind.ASSIGN_DECREMENT:
                bytecode.append(self.inst(get_kind, args=[index]))
                bytecode.append(self.inst(InstKind.SWAP))
                bytecode.append(self.inst(InstKind.SUB))
                bytecode.append(self.inst(set_kind, args=[index]))
            case OpKind.ASSIGN_INCREMENT:
                bytecode.append(self.inst(get_kind, args=[index]))
                bytecode.append(self.inst(InstKind.ADD))
                bytecode.append(self.inst(set_kind, args=[index]))
            case OpKind.ASSIGN_VARIABLE:
                bytecode.append(self.inst(set_kind, args=[index]))

    def _compile_if_block(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile IF_START, IF_CONDITION, IF_ELIF, IF_ELSE, IF_END ops."""
        match op.kind:
            case OpKind.IF_CONDITION:
                self._require_matching_label(
                    op,
                    OpKind.IF_START,
                    OpKind.IF_END,
                    "`then` without matching `if`",
                    reverse=True,
                )
                end_label = self._require_matching_label(
                    op,
                    OpKind.IF_START,
                    OpKind.IF_END,
                    "`then` without matching `fi`",
                    target_kinds=[OpKind.IF_ELIF, OpKind.IF_ELSE, OpKind.IF_END],
                )
                bytecode.append(self.inst(InstKind.JUMP_NE, args=[end_label]))
            case OpKind.IF_ELIF:
                self._require_matching_label(
                    op,
                    OpKind.IF_START,
                    OpKind.IF_END,
                    "`elif` without parent `if`",
                    reverse=True,
                )
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
                    op,
                    OpKind.IF_START,
                    OpKind.IF_END,
                    "`elif` without matching `fi`",
                )
                bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
                bytecode.append(self.inst(InstKind.LABEL, args=[elif_label]))
            case OpKind.IF_ELSE:
                self._require_matching_label(
                    op,
                    OpKind.IF_START,
                    OpKind.IF_END,
                    "`else` without parent `if`",
                    reverse=True,
                )
                else_label = op_to_label(op)
                end_label = self._require_matching_label(
                    op,
                    OpKind.IF_START,
                    OpKind.IF_END,
                    "`else` without matching `fi`",
                )
                bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
                bytecode.append(self.inst(InstKind.LABEL, args=[else_label]))
            case OpKind.IF_END:
                self._require_matching_label(
                    op,
                    OpKind.IF_START,
                    OpKind.IF_END,
                    "`fi` without parent `if`",
                    reverse=True,
                )
                label = op_to_label(op)
                bytecode.append(self.inst(InstKind.LABEL, args=[label]))
            case OpKind.IF_START:
                self._require_matching_label(
                    op,
                    OpKind.IF_START,
                    OpKind.IF_END,
                    "`if` without matching `fi`",
                )

    def _compile_while_block(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile WHILE_START, WHILE_CONDITION, WHILE_BREAK, WHILE_CONTINUE, WHILE_END ops."""
        match op.kind:
            case OpKind.WHILE_BREAK:
                self._require_matching_label(
                    op,
                    OpKind.WHILE_START,
                    OpKind.WHILE_END,
                    "`break` without parent `while`",
                    reverse=True,
                )
                end_label = self._require_matching_label(
                    op,
                    OpKind.WHILE_START,
                    OpKind.WHILE_END,
                    "`break` without matching `done`",
                )
                bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
            case OpKind.WHILE_CONDITION:
                self._require_matching_label(
                    op,
                    OpKind.WHILE_START,
                    OpKind.WHILE_END,
                    "`do` without parent `while`",
                    reverse=True,
                )
                end_label = self._require_matching_label(
                    op,
                    OpKind.WHILE_START,
                    OpKind.WHILE_END,
                    "`do` without matching `done`",
                )
                bytecode.append(self.inst(InstKind.JUMP_NE, args=[end_label]))
            case OpKind.WHILE_CONTINUE:
                continue_target = self._require_matching_label(
                    op,
                    OpKind.WHILE_START,
                    OpKind.WHILE_END,
                    "`continue` without parent `while`",
                    reverse=True,
                )
                bytecode.append(self.inst(InstKind.JUMP, args=[continue_target]))
            case OpKind.WHILE_END:
                while_label = op_to_label(op)
                loop_start = self._require_matching_label(
                    op,
                    OpKind.WHILE_START,
                    OpKind.WHILE_END,
                    "`done` without parent `while`",
                    reverse=True,
                )
                bytecode.append(self.inst(InstKind.JUMP, args=[loop_start]))
                bytecode.append(self.inst(InstKind.LABEL, args=[while_label]))
            case OpKind.WHILE_START:
                label = op_to_label(op)
                bytecode.append(self.inst(InstKind.LABEL, args=[label]))

    def _compile_function_ops(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile FN_CALL and FN_PUSH ops."""
        match op.kind:
            case OpKind.FN_CALL:
                function_name = op.value
                function = GLOBAL_FUNCTIONS.get(function_name)
                assert isinstance(function, Function), "Expected function"
                self._compile_function(function, op)
                bytecode.append(self.inst(InstKind.FN_CALL, args=[function_name]))
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
                for index, capture in enumerate(lambda_function.captures):
                    if self.function and capture in self.function.variables:
                        index = self.function.variables.index(capture)
                        bytecode.append(self.inst(InstKind.LOCAL_GET, args=[index]))
                    elif capture in GLOBAL_VARIABLES:
                        index = list(GLOBAL_VARIABLES.values()).index(capture)
                        bytecode.append(self.inst(InstKind.GLOBAL_GET, args=[index]))
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
                bytecode.append(self.inst(InstKind.FN_PUSH, args=[function_name]))

    def _compile_push_array(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile PUSH_ARRAY op."""
        assert isinstance(op.value, list), "Expected `list`"
        list_len = len(op.value)

        # Push array items in the reverse order
        reversed_items: list[Op] = list(reversed(op.value))
        list_compiler = Compiler(
            reversed_items,
            string_table=self.string_table,
            constants_table=self.constants_table,
        )
        bytecode += list_compiler.compile()

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
        bytecode.append(self.inst(InstKind.GT))
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

    def _compile_match_block(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile MATCH_START, MATCH_ARM, MATCH_END ops."""
        match op.kind:
            case OpKind.MATCH_START:
                pass
            case OpKind.MATCH_ARM:
                pattern = op.value
                assert isinstance(pattern, (EnumVariant, StructPattern, LiteralPattern))

                end_label = self._require_matching_label(
                    op,
                    OpKind.MATCH_START,
                    OpKind.MATCH_END,
                    "`match` arm without matching `end`",
                )
                is_first = id(op) not in _op_label_map

                if not is_first:
                    # End of previous arm body: jump to end, then label for this arm
                    bytecode.append(self.inst(InstKind.JUMP, args=[end_label]))
                    bytecode.append(
                        self.inst(InstKind.LABEL, args=[_op_label_map[id(op)]])
                    )

                next_arm = self.find_matching_label(
                    op=op,
                    start_kind=OpKind.MATCH_START,
                    end_kind=OpKind.MATCH_END,
                    target_kinds=[OpKind.MATCH_ARM],
                )
                is_last = next_arm is None

                if isinstance(pattern, StructPattern):
                    self._compile_struct_match_arm(pattern, bytecode)
                elif isinstance(pattern, LiteralPattern):
                    self._compile_literal_match_arm(
                        pattern, is_last, next_arm, bytecode
                    )
                elif (
                    isinstance(pattern, EnumVariant)
                    and pattern.enum_name
                    and GLOBAL_ENUMS[pattern.enum_name].has_inner_values
                ):
                    self._compile_enum_match_arm(pattern, is_last, next_arm, bytecode)
                elif is_last:
                    bytecode.append(self.inst(InstKind.DROP))
                else:
                    bytecode.append(self.inst(InstKind.DUP))
                    bytecode.append(self.inst(InstKind.PUSH, args=[pattern.ordinal]))
                    bytecode.append(self.inst(InstKind.EQ))
                    bytecode.append(self.inst(InstKind.JUMP_NE, args=[next_arm]))
                    bytecode.append(self.inst(InstKind.DROP))

            case OpKind.MATCH_END:
                end_label = op_to_label(op)
                bytecode.append(self.inst(InstKind.LABEL, args=[end_label]))

    def _compile_enum_variant(
        self,
        variant: EnumVariant,
        casa_enum: "CasaEnum",
        bytecode: list[Inst],
    ) -> None:
        """Compile a data-carrying enum variant construction."""
        inner_types = casa_enum.variant_types.get(variant.variant_name, [])
        slot_count = 1 + len(inner_types)  # ordinal + inner values

        # Store inner values from stack into temp locals (reverse order)
        temp_locals: list[int] = []
        for _ in inner_types:
            local_idx = self.locals_count
            self.locals_count += 1
            temp_locals.append(local_idx)
            bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_idx]))

        # Allocate heap: slot_count * 8 bytes
        local_ptr = self.locals_count
        self.locals_count += 1
        bytecode.append(self.inst(InstKind.PUSH, args=[slot_count * 8]))
        bytecode.append(self.inst(InstKind.HEAP_ALLOC))
        bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_ptr]))

        # Store ordinal at offset 0
        bytecode.append(self.inst(InstKind.PUSH, args=[variant.ordinal]))
        bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_ptr]))
        bytecode.append(self.inst(InstKind.STORE64))

        # Store inner values at offsets 8, 16, ...
        for i, local_idx in enumerate(reversed(temp_locals)):
            offset = (i + 1) * 8
            bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_ptr]))
            bytecode.append(self.inst(InstKind.PUSH, args=[offset]))
            bytecode.append(self.inst(InstKind.ADD))
            bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_idx]))
            bytecode.append(self.inst(InstKind.SWAP))
            bytecode.append(self.inst(InstKind.STORE64))

        # Push ptr
        bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_ptr]))

    def _compile_enum_comparison(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile comparison for data-carrying enums (compare ordinals from heap)."""
        # Stack: [enum_ptr_a, enum_ptr_b] -> load ordinals and compare
        local_a = self.locals_count
        self.locals_count += 1
        bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_a]))
        # Load ordinal from top value (b)
        bytecode.append(self.inst(InstKind.LOAD64))
        # Load ordinal from saved value (a)
        bytecode.append(self.inst(InstKind.LOCAL_GET, args=[local_a]))
        bytecode.append(self.inst(InstKind.LOAD64))
        # Compare ordinals
        inst_kind = InstKind.EQ if op.kind == OpKind.EQ else InstKind.NE
        bytecode.append(self.inst(inst_kind))

    def _compile_is_check(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile IS_CHECK: variant check that pushes bool, optionally extracts bindings."""
        variant = op.value
        assert isinstance(variant, EnumVariant)
        assert variant.enum_name is not None
        casa_enum = GLOBAL_ENUMS[variant.enum_name]

        if not casa_enum.has_inner_values:
            # Plain enum: stack has ordinal, compare directly
            bytecode.append(self.inst(InstKind.PUSH, args=[variant.ordinal]))
            bytecode.append(self.inst(InstKind.EQ))
            return

        if not variant.bindings:
            # Data-carrying enum, no bindings: load ordinal and compare
            bytecode.append(self.inst(InstKind.LOAD64))
            bytecode.append(self.inst(InstKind.PUSH, args=[variant.ordinal]))
            bytecode.append(self.inst(InstKind.EQ))
            return

        # Data-carrying enum with bindings: save ptr, check ordinal,
        # conditionally extract bindings
        temp_ptr = self.locals_count
        self.locals_count += 1

        skip_bindings = new_label()

        # Save the enum pointer
        bytecode.append(self.inst(InstKind.LOCAL_SET, args=[temp_ptr]))

        # Load ordinal and compare
        bytecode.append(self.inst(InstKind.LOCAL_GET, args=[temp_ptr]))
        bytecode.append(self.inst(InstKind.LOAD64))
        bytecode.append(self.inst(InstKind.PUSH, args=[variant.ordinal]))
        bytecode.append(self.inst(InstKind.EQ))

        # Duplicate bool: one for binding extraction check, one stays on stack
        bytecode.append(self.inst(InstKind.DUP))
        bytecode.append(self.inst(InstKind.JUMP_NE, args=[skip_bindings]))

        # Extract bindings from inner values
        for i, var_name in enumerate(variant.bindings):
            offset = (i + 1) * 8
            bytecode.append(self.inst(InstKind.LOCAL_GET, args=[temp_ptr]))
            bytecode.append(self.inst(InstKind.PUSH, args=[offset]))
            bytecode.append(self.inst(InstKind.ADD))
            bytecode.append(self.inst(InstKind.LOAD64))
            _, set_kind, var_index = self._resolve_variable(var_name)
            bytecode.append(self.inst(set_kind, args=[var_index]))

        bytecode.append(self.inst(InstKind.LABEL, args=[skip_bindings]))

    def _compile_enum_match_arm(
        self,
        pattern: EnumVariant,
        is_last: bool,
        next_arm: LabelId | None,
        bytecode: list[Inst],
    ) -> None:
        """Compile a data-carrying enum match arm."""
        assert pattern.enum_name is not None
        casa_enum = GLOBAL_ENUMS[pattern.enum_name]

        if not is_last and not pattern.is_wildcard:
            # Compare ordinal: DUP ptr, LOAD64 tag, compare
            bytecode.append(self.inst(InstKind.DUP))
            bytecode.append(self.inst(InstKind.LOAD64))
            bytecode.append(self.inst(InstKind.PUSH, args=[pattern.ordinal]))
            bytecode.append(self.inst(InstKind.EQ))
            bytecode.append(self.inst(InstKind.JUMP_NE, args=[next_arm]))

        # Extract bindings from inner values
        for i, var_name in enumerate(pattern.bindings):
            offset = (i + 1) * 8
            bytecode.append(self.inst(InstKind.DUP))
            bytecode.append(self.inst(InstKind.PUSH, args=[offset]))
            bytecode.append(self.inst(InstKind.ADD))
            bytecode.append(self.inst(InstKind.LOAD64))
            _, set_kind, var_index = self._resolve_variable(var_name)
            bytecode.append(self.inst(set_kind, args=[var_index]))

        # Drop the enum pointer
        bytecode.append(self.inst(InstKind.DROP))

    def _compile_literal_match_arm(
        self,
        pattern: LiteralPattern,
        is_last: bool,
        next_arm: LabelId | None,
        bytecode: list[Inst],
    ) -> None:
        """Compile a literal value match arm (bool, int, char, str)."""
        if is_last:
            bytecode.append(self.inst(InstKind.DROP))
            return
        bytecode.append(self.inst(InstKind.DUP))
        if pattern.typ == "str":
            assert isinstance(pattern.value, str)
            string_index = self.intern_string(pattern.value)
            bytecode.append(self.inst(InstKind.PUSH_STR, args=[string_index]))
            bytecode.append(self.inst(InstKind.STR_EQ))
        elif pattern.typ == "char":
            bytecode.append(self.inst(InstKind.PUSH, args=[pattern.value]))
            bytecode.append(self.inst(InstKind.EQ))
        else:
            bytecode.append(self.inst(InstKind.PUSH, args=[int(pattern.value)]))
            bytecode.append(self.inst(InstKind.EQ))
        bytecode.append(self.inst(InstKind.JUMP_NE, args=[next_arm]))
        bytecode.append(self.inst(InstKind.DROP))

    def _compile_struct_new(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile STRUCT_NEW op."""
        struct = op.value
        assert isinstance(struct, Struct), "Expected struct"
        member_count = len(struct.members)
        bytecode.append(self.inst(InstKind.PUSH, args=[member_count * 8]))
        bytecode.append(self.inst(InstKind.HEAP_ALLOC))
        for i in range(member_count):
            bytecode.append(self.inst(InstKind.SWAP))
            bytecode.append(self.inst(InstKind.OVER))
            if i > 0:
                bytecode.append(self.inst(InstKind.PUSH, args=[i * 8]))
                bytecode.append(self.inst(InstKind.ADD))
            bytecode.append(self.inst(InstKind.STORE64))

    def _compile_struct_match_arm(
        self,
        pattern: StructPattern,
        bytecode: list[Inst],
    ) -> None:
        """Compile a struct destructuring match arm."""
        struct = GLOBAL_STRUCTS[pattern.struct_name]
        field_offsets = {m.name: i * 8 for i, m in enumerate(struct.members)}

        # Extract bound fields from the struct pointer
        for field_name, var_name in pattern.bindings.items():
            offset = field_offsets[field_name]
            bytecode.append(self.inst(InstKind.DUP))
            if offset > 0:
                bytecode.append(self.inst(InstKind.PUSH, args=[offset]))
                bytecode.append(self.inst(InstKind.ADD))
            bytecode.append(self.inst(InstKind.LOAD64))
            _, set_kind, var_index = self._resolve_variable(var_name)
            bytecode.append(self.inst(set_kind, args=[var_index]))

        # Drop the struct pointer
        bytecode.append(self.inst(InstKind.DROP))

    def _compile_struct_literal(self, op: Op, bytecode: list[Inst]) -> None:
        """Compile STRUCT_LITERAL op with named fields."""
        literal = op.value
        assert isinstance(literal, StructLiteral)
        struct = literal.struct
        member_count = len(struct.members)

        # Map field names to their declaration index
        decl_index_by_name = {m.name: i for i, m in enumerate(struct.members)}

        # Store all field values into temp locals (top of stack = last in field_order)
        temp_locals: dict[str, int] = {}
        for field_name in reversed(literal.field_order):
            local_idx = self.locals_count
            self.locals_count += 1
            temp_locals[field_name] = local_idx
            bytecode.append(self.inst(InstKind.LOCAL_SET, args=[local_idx]))

        # Allocate struct on heap
        bytecode.append(self.inst(InstKind.PUSH, args=[member_count * 8]))
        bytecode.append(self.inst(InstKind.HEAP_ALLOC))

        # Store each field at its correct offset
        for member in struct.members:
            offset = decl_index_by_name[member.name] * 8
            bytecode.append(self.inst(InstKind.DUP))
            if offset > 0:
                bytecode.append(self.inst(InstKind.PUSH, args=[offset]))
                bytecode.append(self.inst(InstKind.ADD))
            bytecode.append(
                self.inst(InstKind.LOCAL_GET, args=[temp_locals[member.name]])
            )
            bytecode.append(self.inst(InstKind.SWAP))
            bytecode.append(self.inst(InstKind.STORE64))

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
                    f"Function `{function.name}` assigns global"
                    f" variable `{var.name}` before it is"
                    " initialized within the global scope",
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
        """Lower all ops to bytecode instructions."""
        assert len(InstKind) == 69, "Exhaustive handling for `InstructionKind"
        assert len(OpKind) == 86, "Exhaustive handling for `OpKind`"

        cursor = Cursor(sequence=self.ops)
        bytecode: list[Inst] = []
        if self.function:
            self.locals_count = len(self.function.variables)

        # Function label
        fn_label = new_label()
        bytecode.append(Inst(InstKind.LABEL, args=[fn_label]))

        while op := cursor.pop():
            self._current_loc = op.location
            if op.kind in DIRECT_OP_TO_INST:
                # Data-carrying enum comparison: load ordinals from heap
                if (
                    op.kind in (OpKind.EQ, OpKind.NE)
                    and op.type_annotation
                    and op.type_annotation in GLOBAL_ENUMS
                ):
                    self._compile_enum_comparison(op, bytecode)
                    continue
                # String content comparison
                if op.kind in (OpKind.EQ, OpKind.NE) and op.type_annotation == "str":
                    bytecode.append(self.inst(InstKind.STR_EQ))
                    if op.kind == OpKind.NE:
                        bytecode.append(self.inst(InstKind.NOT))
                    continue
                # Data-carrying enum print: load ordinal from heap
                if op.kind == OpKind.PRINT_INT and op.type_annotation:
                    bytecode.append(self.inst(InstKind.LOAD64))
                    bytecode.append(self.inst(InstKind.PRINT_INT))
                    continue
                bytecode.append(self.inst(DIRECT_OP_TO_INST[op.kind]))
                continue
            match op.kind:
                case (
                    OpKind.ASSIGN_DECREMENT
                    | OpKind.ASSIGN_INCREMENT
                    | OpKind.ASSIGN_VARIABLE
                ):
                    self._compile_assignment(op, bytecode)
                case OpKind.FSTRING_CONCAT:
                    count = op.value
                    assert isinstance(count, int)
                    bytecode.append(self.inst(InstKind.FSTRING_CONCAT, args=[count]))
                case OpKind.FN_CALL | OpKind.FN_PUSH:
                    if op.kind == OpKind.FN_PUSH:
                        fn = GLOBAL_FUNCTIONS.get(op.value)
                        if fn and fn.has_deferred_methods:
                            # Deferred lambda: push dummy, real calls are
                            # specialized at each exec site
                            bytecode.append(self.inst(InstKind.PUSH, args=[0]))
                            continue
                    self._compile_function_ops(op, bytecode)
                case OpKind.IDENTIFIER:
                    raise AssertionError(
                        f"Identifier `{op.value}` should be resolved by the parser"
                    )
                case (
                    OpKind.IF_CONDITION
                    | OpKind.IF_ELIF
                    | OpKind.IF_ELSE
                    | OpKind.IF_END
                    | OpKind.IF_START
                ):
                    self._compile_if_block(op, bytecode)
                case OpKind.INCLUDE_FILE | OpKind.TYPE_CAST:
                    pass
                case OpKind.PUSH_ENUM_VARIANT:
                    variant = op.value
                    assert isinstance(variant, EnumVariant)
                    assert variant.enum_name is not None
                    casa_enum = GLOBAL_ENUMS[variant.enum_name]
                    if casa_enum.has_inner_values:
                        self._compile_enum_variant(variant, casa_enum, bytecode)
                    else:
                        bytecode.append(
                            self.inst(InstKind.PUSH, args=[variant.ordinal])
                        )
                case OpKind.IS_CHECK:
                    self._compile_is_check(op, bytecode)
                case OpKind.MATCH_START | OpKind.MATCH_ARM | OpKind.MATCH_END:
                    self._compile_match_block(op, bytecode)
                case OpKind.METHOD_CALL:
                    raise AssertionError(
                        "Method call should be transpiled to function call during type checking"
                    )
                case OpKind.PRINT:
                    assert (
                        False
                    ), "PRINT should be resolved to a PRINT variant by the type checker"
                case OpKind.TYPEOF:
                    type_name = op.type_annotation
                    assert isinstance(type_name, str), "typeof requires type_annotation"
                    string_index = self.intern_string(type_name)
                    bytecode.append(self.inst(InstKind.DROP))
                    bytecode.append(self.inst(InstKind.PUSH_STR, args=[string_index]))
                    bytecode.append(self.inst(InstKind.PRINT_STR))
                case OpKind.PUSH_ARRAY:
                    self._compile_push_array(op, bytecode)
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
                case OpKind.PUSH_STR:
                    string_index = self.intern_string(op.value)
                    bytecode.append(self.inst(InstKind.PUSH_STR, args=[string_index]))
                case OpKind.PUSH_VARIABLE:
                    variable_name = op.value
                    assert isinstance(variable_name, str), "Valid variable name"
                    get_kind, _, index = self._resolve_variable(variable_name)
                    bytecode.append(self.inst(get_kind, args=[index]))
                case OpKind.STRUCT_NEW:
                    self._compile_struct_new(op, bytecode)
                case OpKind.STRUCT_LITERAL:
                    self._compile_struct_literal(op, bytecode)
                case (
                    OpKind.WHILE_BREAK
                    | OpKind.WHILE_CONDITION
                    | OpKind.WHILE_CONTINUE
                    | OpKind.WHILE_END
                    | OpKind.WHILE_START
                ):
                    self._compile_while_block(op, bytecode)
                case _:
                    raise AssertionError(f"Unhandled OpKind: {op.kind}")

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
        """Search for a matching block boundary and return its label."""
        START_KINDS = [OpKind.IF_START, OpKind.WHILE_START, OpKind.MATCH_START]
        END_KINDS = [OpKind.IF_END, OpKind.WHILE_END, OpKind.MATCH_END]
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

            if depth < START_DEPTH:
                stop_kind = start_kind if reverse else end_kind
                if other_op.kind == stop_kind:
                    return None

        return None
