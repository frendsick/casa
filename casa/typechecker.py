"""Stack-based type inference and checking for the Casa compiler."""

from dataclasses import dataclass, field
from collections.abc import Iterable
from typing import assert_never

from casa.common import (
    ANY_TYPE,
    GLOBAL_ENUMS,
    GLOBAL_FUNCTIONS,
    GLOBAL_STRUCTS,
    GLOBAL_TRAITS,
    GLOBAL_VARIABLES,
    LITERAL_MATCH_TYPES,
    MATCH_WILDCARD,
    EnumVariant,
    Function,
    Intrinsic,
    LiteralPattern,
    Location,
    Op,
    OpKind,
    Parameter,
    Signature,
    Struct,
    StructLiteral,
    StructPattern,
    Type,
    Variable,
    extract_fn_signature_str,
    extract_generic_base,
    extract_generic_inner,
    extract_generic_params,
    is_fn_type,
    resolve_trait_sig,
)
from casa.error import (
    WARNINGS,
    CasaError,
    CasaErrorCollection,
    CasaWarning,
    ErrorKind,
    WarningKind,
    raise_error,
)
from casa.parser import resolve_identifiers


@dataclass
class BranchedStack:
    """Tracks stack state across conditional and loop branches."""

    before: list[Type]
    after: list[Type]
    before_origins: list[Location | None]
    after_origins: list[Location | None]
    condition_present: bool
    default_present: bool
    # Per-branch tracking for better error messages
    branch_records: list[tuple[str, list[Type], Location]]
    current_branch_label: str | None
    current_branch_location: Location | None
    if_location: Location | None
    if_location_match_type: str | None

    def __init__(
        self,
        before: list[Type],
        before_origins: list[Location | None],
        after: list[Type] | None = None,
        after_origins: list[Location | None] | None = None,
    ):
        self.before = before.copy()
        self.before_origins = before_origins.copy()
        self.after = after.copy() if after else before.copy()
        self.after_origins = (
            after_origins.copy() if after_origins else before_origins.copy()
        )
        self.default_present = False
        self.condition_present = False
        self.branch_records = []
        self.current_branch_label = None
        self.current_branch_location = None
        self.if_location = None
        self.if_location_match_type = None


@dataclass
class TypeChecker:
    """Simulates the stack symbolically to infer and verify types."""

    ops: list[Op]
    stack: list[Type] = field(default_factory=list)
    stack_origins: list[Location | None] = field(default_factory=list)
    parameters: list[Parameter] = field(default_factory=list)
    # Saved on `return`
    return_types: list[Type] | None = None
    # Store stack states before each conditional and loop block
    branched_stacks: list[BranchedStack] = field(default_factory=list)
    # Current op location for error reporting
    current_location: Location | None = None
    # Origin of the last popped value (for error notes)
    last_pop_origin: Location | None = None
    # Human-readable stack effect of the current operation (for error message)
    current_op_context: str | None = None
    # Which parameter mismatched (for error expected line)
    current_expect_context: str | None = None
    # Maps variable names to lambda function names for deferred method lambdas
    deferred_lambda_vars: dict[str, str] = field(default_factory=dict)
    # Tracks the last pushed deferred lambda name for assignment tracking
    last_deferred_lambda: str | None = None
    # True when processing ops between IF_START/IF_ELIF and IF_CONDITION (then)
    in_branch_condition: bool = False

    def stack_push(self, typ: Type, origin: Location | None = None):
        """Push a type onto the symbolic stack."""
        self.stack.append(typ)
        self.stack_origins.append(origin or self.current_location)

    def stack_peek(self) -> Type:
        """Return the top type without popping."""
        if not self.stack:
            return ANY_TYPE
        return self.stack[-1]

    def stack_pop(self) -> Type:
        """Pop and return the top type from the symbolic stack."""
        if not self.stack:
            self.parameters.append(Parameter(ANY_TYPE))
            self.last_pop_origin = None
            return ANY_TYPE
        self.last_pop_origin = self.stack_origins.pop()
        return self.stack.pop()

    def expect_type(self, expected: Type) -> Type:
        """Pop and verify the top type matches expected."""
        if not self.stack:
            self.parameters.append(Parameter(expected))
            self.last_pop_origin = None
            return expected

        origin = self.stack_origins.pop()
        typ = self.stack.pop()
        self.last_pop_origin = origin
        if (
            expected == ANY_TYPE
            or (expected == "fn" and is_fn_type(typ))
            or extract_generic_base(typ) == expected
        ):
            return typ
        if (
            typ == ANY_TYPE
            or (typ == "fn" and is_fn_type(expected))
            or extract_generic_base(expected) == typ
        ):
            return expected
        if typ == expected:
            return typ
        # Match parameterized generics with any/type-var wildcard
        exp_base = extract_generic_base(expected)
        act_base = extract_generic_base(typ)
        if exp_base is not None and exp_base == act_base:
            exp_params = extract_generic_params(expected)
            act_params = extract_generic_params(typ)
            type_vars: set[str] = set()
            if exp_base in GLOBAL_ENUMS:
                type_vars = set(GLOBAL_ENUMS[exp_base].type_vars)
            if exp_params and act_params and len(exp_params) == len(act_params):
                if all(
                    expected_param == ANY_TYPE
                    or actual_param == ANY_TYPE
                    or expected_param in type_vars
                    or actual_param in type_vars
                    or expected_param == actual_param
                    for expected_param, actual_param in zip(exp_params, act_params)
                ):
                    return expected
            else:
                exp_inner = extract_generic_inner(expected)
                act_inner = extract_generic_inner(typ)
                if ANY_TYPE in (exp_inner, act_inner):
                    return expected
        # Match fn[sig] types using Signature.matches (handles ANY_TYPE)
        exp_sig_str = extract_fn_signature_str(expected)
        act_sig_str = extract_fn_signature_str(typ)
        if exp_sig_str and act_sig_str:
            exp_sig = Signature.from_str(exp_sig_str)
            act_sig = Signature.from_str(act_sig_str)
            if exp_sig.matches(act_sig):
                return expected
        if expected == "fn" and typ.startswith("fn["):
            signature = Signature.from_str(typ[3:-1])
            if signature.parameters != signature.return_types:
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    "Expected symmetrical function type",
                    self.current_location,
                )
            return typ
        message = "Type mismatch"
        if self.current_op_context:
            message = f"Type mismatch in {self.current_op_context}"
        expected_str = f"`{expected}`"
        if self.current_expect_context:
            expected_str = f"`{expected}` ({self.current_expect_context})"
        notes = []
        if origin and origin != self.current_location:
            notes = [(f"value of type `{typ}` pushed here", origin)]
        raise_error(
            ErrorKind.TYPE_MISMATCH,
            message,
            self.current_location,
            expected=expected_str,
            got=f"`{typ}`",
            notes=notes,
        )

    def _bind_type_var(
        self,
        bindings: dict[str, Type],
        type_var: str,
        actual: Type,
    ):
        """Bind a type variable to a concrete type, checking consistency."""
        if type_var not in bindings:
            bindings[type_var] = actual
            return

        bound = bindings[type_var]
        if (bound == ANY_TYPE or _is_enum_type_var(bound)) and actual != ANY_TYPE:
            bindings[type_var] = actual
        elif (
            actual != bound
            and actual != ANY_TYPE
            and bound != ANY_TYPE
            and not _is_enum_type_var(actual)
        ):
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"Type variable `{type_var}` bound to `{bound}` but got `{actual}`",
                self.current_location,
            )

    def apply_signature(self, signature: Signature, name: str | None = None):
        """Pop parameters and push return types according to a signature."""
        self.current_op_context = f"`{name}` ({signature})" if name else None

        if not signature.type_vars:
            for i, expected in enumerate(signature.parameters):
                if name:
                    self.current_expect_context = f"parameter {i + 1} of `{name}`"
                self.expect_type(expected.typ)
            self.current_expect_context = None
            for return_type in signature.return_types:
                self.stack_push(return_type)
            return

        # Bind type variables to actual stack types
        bindings: dict[str, Type] = {}
        for i, expected in enumerate(signature.parameters):
            if name:
                self.current_expect_context = f"parameter {i + 1} of `{name}`"
            if expected.typ in signature.type_vars:
                self._bind_type_var(bindings, expected.typ, self.stack_pop())
                continue
            if self._bind_generic_param(bindings, expected, signature):
                continue
            if self._bind_fn_param(bindings, expected, signature):
                continue
            self.expect_type(expected.typ)
        self.current_expect_context = None

        for return_type in signature.return_types:
            resolved = _resolve_return_type(return_type, bindings)
            self.stack_push(resolved)

    def _bind_generic_param(
        self,
        bindings: dict[str, Type],
        expected: Parameter,
        signature: Signature,
    ) -> bool:
        """Bind type vars from a parameterized param like Map[K V].

        Returns True if this parameter was handled.
        """
        generic_base = extract_generic_base(expected.typ)
        if not generic_base:
            return False
        exp_params = extract_generic_params(expected.typ)
        if not exp_params or not any(p in signature.type_vars for p in exp_params):
            return False

        actual = self.stack_pop()
        actual_base = extract_generic_base(actual)
        if actual_base == generic_base:
            act_params = extract_generic_params(actual)
            if act_params and len(act_params) == len(exp_params):
                for expected_param, actual_param in zip(exp_params, act_params):
                    if expected_param in signature.type_vars:
                        self._bind_type_var(bindings, expected_param, actual_param)
                return True
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                "Type mismatch",
                self.current_location,
                expected=f"`{expected.typ}`",
                got=f"`{actual}`",
            )
        if actual in (generic_base, ANY_TYPE):
            for ep in exp_params:
                if ep in signature.type_vars:
                    self._bind_type_var(bindings, ep, ANY_TYPE)
            return True
        raise_error(
            ErrorKind.TYPE_MISMATCH,
            "Type mismatch",
            self.current_location,
            expected=f"`{expected.typ}`",
            got=f"`{actual}`",
        )

    def _bind_fn_param(
        self,
        bindings: dict[str, Type],
        expected: Parameter,
        signature: Signature,
    ) -> bool:
        """Bind type vars from a fn[sig] param like fn[T -> T].

        Returns True if this parameter was handled.
        """
        fn_sig_str = extract_fn_signature_str(expected.typ)
        if not fn_sig_str or not any(
            type_var in fn_sig_str for type_var in signature.type_vars
        ):
            return False

        actual = self.stack_pop()
        if actual == ANY_TYPE:
            for type_var in signature.type_vars:
                if type_var in fn_sig_str:
                    self._bind_type_var(bindings, type_var, ANY_TYPE)
            return True
        if not is_fn_type(actual):
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                "Type mismatch",
                self.current_location,
                expected=f"`{expected.typ}`",
                got=f"`{actual}`",
            )
        actual_sig_str = extract_fn_signature_str(actual)
        if not actual_sig_str:
            return True
        exp_fn_sig = Signature.from_str(fn_sig_str)
        act_fn_sig = Signature.from_str(actual_sig_str)
        for expected_param, actual_param in zip(
            exp_fn_sig.parameters, act_fn_sig.parameters
        ):
            if expected_param.typ in signature.type_vars:
                self._bind_type_var(bindings, expected_param.typ, actual_param.typ)
        for er, ar in zip(exp_fn_sig.return_types, act_fn_sig.return_types):
            if er in signature.type_vars:
                self._bind_type_var(bindings, er, ar)
        return True

    def check_arithmetic(self, op: Op) -> None:
        """Handle ADD, SUB, MUL, DIV, MOD."""
        match op.kind:
            case OpKind.ADD | OpKind.SUB:
                self.expect_type("int")
                t1 = self.stack_pop()
                self.stack_push(t1)
            case OpKind.DIV | OpKind.MOD | OpKind.MUL:
                self.expect_type("int")
                self.expect_type("int")
                self.stack_push("int")

    def check_comparison(self, op: Op) -> None:
        """Handle EQ, NE, LT, LE, GT, GE."""
        t1 = self.stack_pop()
        t2 = self.stack_pop()
        t1_base = self._resolve_match_type(t1)
        t1_enum = t1_base if t1_base in GLOBAL_ENUMS else None
        t2_base = self._resolve_match_type(t2)
        t2_enum = t2_base if t2_base in GLOBAL_ENUMS else None
        if t1_enum or t2_enum:
            if t1 != t2:
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    f"Cannot compare `{t2}` with `{t1}`",
                    self.current_location,
                )
            if op.kind not in (OpKind.EQ, OpKind.NE):
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    f"Enum type `{t1}` only supports `==` and `!=` comparison",
                    self.current_location,
                )
            # Annotate for bytecode: data-carrying enums need heap tag comparison
            assert t1_enum is not None
            casa_enum = GLOBAL_ENUMS[t1_enum]
            if casa_enum.has_inner_values:
                op.type_annotation = t1_enum
        elif t1 == "str" or t2 == "str":
            if t1 != t2:
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    f"Cannot compare `{t2}` with `{t1}`",
                    self.current_location,
                )
            if op.kind not in (OpKind.EQ, OpKind.NE):
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    "Type `str` only supports `==` and `!=` comparison",
                    self.current_location,
                )
            op.type_annotation = "str"
        self.stack_push("bool")

    def check_boolean(self, op: Op) -> None:
        """Handle AND, OR, NOT."""
        if op.kind == OpKind.NOT:
            self.stack_pop()
        else:
            self.stack_pop()
            self.stack_pop()
        self.stack_push("bool")

    def check_bitwise(self, op: Op) -> None:
        """Handle BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, SHL, SHR."""
        if op.kind == OpKind.BIT_NOT:
            self.expect_type("int")
            self.stack_push("int")
        elif op.kind in (OpKind.SHL, OpKind.SHR):
            self.expect_type("int")
            t1 = self.stack_pop()
            self.stack_push(t1)
        else:
            self.expect_type("int")
            self.expect_type("int")
            self.stack_push("int")

    def check_stack_ops(self, op: Op) -> None:
        """Handle DROP, DUP, SWAP, OVER, ROT."""
        match op.kind:
            case OpKind.DROP:
                self.stack_pop()
            case OpKind.DUP:
                t1 = self.stack_pop()
                origin = self.last_pop_origin
                self.stack_push(t1, origin)
                self.stack_push(t1, origin)
            case OpKind.SWAP:
                t1 = self.stack_pop()
                first_origin = self.last_pop_origin
                t2 = self.stack_pop()
                second_origin = self.last_pop_origin
                self.stack_push(t1, first_origin)
                self.stack_push(t2, second_origin)
            case OpKind.OVER:
                t1 = self.stack_pop()
                first_origin = self.last_pop_origin
                t2 = self.stack_pop()
                second_origin = self.last_pop_origin
                self.stack_push(t2, second_origin)
                self.stack_push(t1, first_origin)
                self.stack_push(t2, second_origin)
            case OpKind.ROT:
                t1 = self.stack_pop()
                first_origin = self.last_pop_origin
                t2 = self.stack_pop()
                second_origin = self.last_pop_origin
                t3 = self.stack_pop()
                third_origin = self.last_pop_origin
                self.stack_push(t2, second_origin)
                self.stack_push(t1, first_origin)
                self.stack_push(t3, third_origin)

    def check_memory(self, op: Op) -> None:
        """Handle LOAD/STORE variants and HEAP_ALLOC."""
        match op.kind:
            case OpKind.LOAD8 | OpKind.LOAD16 | OpKind.LOAD32 | OpKind.LOAD64:
                self.expect_type("ptr")
                self.stack_push("int")
            case OpKind.STORE8 | OpKind.STORE16 | OpKind.STORE32 | OpKind.STORE64:
                self.expect_type("ptr")
                self.stack_pop()
            case OpKind.HEAP_ALLOC:
                self.expect_type("int")
                self.stack_push("ptr")

    def check_assignment(self, op: Op, function: Function | None) -> None:
        """Handle ASSIGN_VARIABLE, ASSIGN_INCREMENT, ASSIGN_DECREMENT."""
        if op.kind in (OpKind.ASSIGN_DECREMENT, OpKind.ASSIGN_INCREMENT):
            self.expect_type("int")
            return

        variable_name = op.value
        assert isinstance(variable_name, str), "Expected variable name"

        # Track deferred lambda variable assignments
        if self.last_deferred_lambda:
            self.deferred_lambda_vars[variable_name] = self.last_deferred_lambda
            self.last_deferred_lambda = None

        stack_type = self.stack_peek()
        effective_type = op.type_annotation if op.type_annotation else stack_type

        if op.type_annotation:
            annotation = op.type_annotation
            if annotation == ANY_TYPE:
                WARNINGS.append(
                    CasaWarning(
                        WarningKind.LOSSY_TYPE_ANNOTATION,
                        f"Type annotation `any` discards type information from `{stack_type}`",
                        op.location,
                    )
                )
            elif (
                extract_generic_base(stack_type) is not None
                and extract_generic_base(annotation) is None
                and extract_generic_base(stack_type) == annotation
            ):
                WARNINGS.append(
                    CasaWarning(
                        WarningKind.LOSSY_TYPE_ANNOTATION,
                        f"Type annotation `{annotation}` discards type"
                        f" parameter from `{stack_type}`",
                        op.location,
                    )
                )
                effective_type = stack_type

        # Local variable (shadows global with same name)
        local_variable = None
        if function:
            for variable in function.variables:
                if variable.name == variable_name:
                    local_variable = variable
                    break

        if local_variable:
            if not local_variable.typ or (
                local_variable.typ == ANY_TYPE and effective_type != ANY_TYPE
            ):
                local_variable.typ = effective_type

            unified = _unify_type(local_variable.typ, effective_type)
            if unified is None:
                raise_error(
                    ErrorKind.INVALID_VARIABLE,
                    f"Cannot override local variable `{local_variable.name}`"
                    f" of type `{local_variable.typ}`"
                    f" with other type `{effective_type}`",
                    op.location,
                )
            local_variable.typ = unified

            self.expect_type(effective_type)
            return

        # Global variable
        global_variable = GLOBAL_VARIABLES.get(variable_name)
        if global_variable:
            assert isinstance(global_variable, Variable), "Valid global variable"

            if not global_variable.typ or (
                global_variable.typ == ANY_TYPE and effective_type != ANY_TYPE
            ):
                global_variable.typ = effective_type

            unified = _unify_type(global_variable.typ, effective_type)
            if unified is None:
                raise_error(
                    ErrorKind.INVALID_VARIABLE,
                    f"Cannot override global variable `{global_variable.name}`"
                    f" of type `{global_variable.typ}`"
                    f" with other type `{effective_type}`",
                    op.location,
                )
            global_variable.typ = unified

            self.expect_type(effective_type)
            return

        raise AssertionError(f"Variable `{variable_name}` is not defined")

    def check_push(self, op: Op, function: Function | None) -> None:
        """Handle PUSH_VARIABLE, PUSH_CAPTURE."""
        if op.kind == OpKind.PUSH_CAPTURE:
            capture_name = op.value
            assert isinstance(op.value, str), "Expected variable name"
            assert isinstance(function, Function), "Expected function"

            if capture_name not in function.captures:
                raise_error(
                    ErrorKind.UNDEFINED_NAME,
                    f"Function `{function.name}` does not have capture `{capture_name}`",
                    op.location,
                )

            index = function.captures.index(capture_name)
            capture = function.captures[index]

            if capture.typ:
                self.stack_push(capture.typ)
                return

            if global_variable := GLOBAL_VARIABLES.get(capture.name):
                assert global_variable.typ, "Variable type"
                capture.typ = global_variable.typ
                self.stack_push(global_variable.typ)
                return

            raise AssertionError(
                f"Capture `{capture.name}` has not been type checked before its usage"
            )

        variable_name = op.value
        assert isinstance(variable_name, str), "Expected variable name"

        # Local variable (shadows global with same name)
        if function:
            for variable in function.variables:
                if variable == variable_name:
                    if not variable.typ:
                        raise AssertionError(
                            f"Variable `{variable.name}` has not been type checked before its usage"
                        )
                    self.stack_push(variable.typ)
                    return

        # Global variable
        global_variable = GLOBAL_VARIABLES.get(variable_name)
        if global_variable:
            assert global_variable.typ, "Global variable type should be defined"
            self.stack_push(global_variable.typ)
            return

        raise_error(
            ErrorKind.UNDEFINED_NAME,
            f"Variable `{variable_name}` is not defined",
            op.location,
        )

    def check_if_block(self, op: Op) -> None:
        """Handle IF_START through IF_END."""
        match op.kind:
            case OpKind.IF_START:
                bs = BranchedStack(self.stack, self.stack_origins)
                bs.if_location = op.location
                self.branched_stacks.append(bs)
                self.in_branch_condition = True
            case OpKind.IF_CONDITION:
                self.in_branch_condition = False
                self.expect_type("bool")

                assert len(self.branched_stacks) > 0, "If block stack state is saved"
                branched = self.branched_stacks[-1]

                if not branched.condition_present:
                    branched.condition_present = True
                    branched.before = self.stack.copy()
                    branched.before_origins = self.stack_origins.copy()
                    branched.after = branched.before
                    branched.after_origins = branched.before_origins
                    branched.current_branch_label = "if"
                    branched.current_branch_location = branched.if_location
                    return

                if self.stack != branched.before:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed across branch",
                        op.location,
                        expected=str(branched.before),
                        got=str(self.stack),
                    )
            case OpKind.IF_ELIF | OpKind.IF_ELSE:
                assert self.branched_stacks, "If block stack state is saved"
                branched = self.branched_stacks[-1]
                before, after = branched.before, branched.after

                if branched.current_branch_label and branched.current_branch_location:
                    branched.branch_records.append(
                        (
                            branched.current_branch_label,
                            self.stack.copy(),
                            branched.current_branch_location,
                        )
                    )

                if op.kind is OpKind.IF_ELSE:
                    branched.default_present = True
                    branched.current_branch_label = "else"
                    self.in_branch_condition = False
                else:
                    branched.current_branch_label = "elif"
                    self.in_branch_condition = True
                branched.current_branch_location = op.location

                if self.stack == before == after:
                    return
                unified_cur_after = _stacks_compatible(self.stack, after)
                if unified_cur_after is not None and before != after:
                    branched.after = unified_cur_after
                    self.stack = before.copy()
                    self.stack_origins = branched.before_origins.copy()
                    return
                if before == after:
                    branched.after = self.stack.copy()
                    branched.after_origins = self.stack_origins.copy()
                    self.stack = before.copy()
                    self.stack_origins = branched.before_origins.copy()
                    return
                raise_error(
                    ErrorKind.STACK_MISMATCH,
                    "Branches have incompatible stack effects",
                    op.location,
                    notes=_branch_mismatch_notes(branched),
                )
            case OpKind.IF_END:
                branched = self.branched_stacks.pop()

                if branched.current_branch_label and branched.current_branch_location:
                    branched.branch_records.append(
                        (
                            branched.current_branch_label,
                            self.stack.copy(),
                            branched.current_branch_location,
                        )
                    )

                if self.stack == branched.before == branched.after:
                    return
                unified_cur_after = _stacks_compatible(self.stack, branched.after)
                if unified_cur_after is not None and branched.default_present:
                    self.stack = unified_cur_after
                    self.stack_origins = branched.after_origins.copy()
                    return
                unified_cur_before = _stacks_compatible(self.stack, branched.before)
                if unified_cur_before is not None and not branched.default_present:
                    self.stack = unified_cur_before
                    self.stack_origins = branched.before_origins.copy()
                    return
                raise_error(
                    ErrorKind.STACK_MISMATCH,
                    "Branches have incompatible stack effects",
                    op.location,
                    notes=_branch_mismatch_notes(branched),
                )

    def check_while_block(self, op: Op) -> None:
        """Handle WHILE_START through WHILE_END."""
        match op.kind:
            case OpKind.WHILE_START:
                self.branched_stacks.append(
                    BranchedStack(self.stack, self.stack_origins)
                )
            case OpKind.WHILE_CONDITION:
                self.expect_type("bool")
                assert len(self.branched_stacks) > 0, "While block stack state is saved"
                branched = self.branched_stacks[-1]
                branched.condition_present = True
                if self.stack != branched.before:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed in loop",
                        op.location,
                        expected=str(branched.before),
                        got=str(self.stack),
                    )
            case OpKind.WHILE_BREAK | OpKind.WHILE_CONTINUE:
                assert len(self.branched_stacks) > 0, "While block stack state is saved"
                branched = self.branched_stacks[-1]
                if self.stack != branched.after:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed in loop",
                        op.location,
                        expected=str(branched.after),
                        got=str(self.stack),
                    )
            case OpKind.WHILE_END:
                branched = self.branched_stacks.pop()
                if branched.after != self.stack:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed in loop",
                        op.location,
                        expected=str(branched.after),
                        got=str(self.stack),
                    )

    def check_function_ops(
        self,
        op: Op,
        ops: list[Op],
        op_index: int,
        function: Function | None,
    ) -> int:
        """Handle FN_CALL, FN_EXEC, FN_PUSH, FN_RETURN. Returns updated op_index."""
        match op.kind:
            case OpKind.FN_CALL:
                function_name = op.value
                assert isinstance(function_name, str), "Expected function name"
                global_function = GLOBAL_FUNCTIONS.get(function_name)
                assert global_function, "Expected function"
                _ensure_typechecked(global_function)
                assert global_function.signature, "Signature is defined"
                fn_sig = global_function.signature
                if fn_sig.trait_bounds:
                    injected = _inject_trait_fn_ptrs(
                        self,
                        fn_sig,
                        ops,
                        op_index,
                        op.location,
                        function,
                    )
                    op_index += injected
                self.apply_signature(fn_sig, function_name)
            case OpKind.FN_EXEC:
                fn_symmetrical = "fn"
                fn_ptr = self.stack_peek()
                if fn_ptr == ANY_TYPE:
                    fn_ptr = "fn"
                # Specialize deferred method lambdas at call site
                _try_specialize_deferred_exec(self, fn_ptr, ops, op_index, op)
                fn_ptr = self.expect_type(fn_ptr)
                assert isinstance(fn_ptr, str), "Function pointer type"
                if fn_ptr != fn_symmetrical:
                    self.apply_signature(Signature.from_str(fn_ptr[3:-1]), "exec")
            case OpKind.FN_PUSH:
                assert isinstance(op.value, str), "Expected identifier name"
                function_name = op.value
                global_function = GLOBAL_FUNCTIONS.get(function_name)
                assert isinstance(global_function, Function), "Expected function"
                _ensure_typechecked(global_function)
                self.stack_push(f"fn[{global_function.signature}]")
                if global_function.has_deferred_methods:
                    self.last_deferred_lambda = function_name
                else:
                    self.last_deferred_lambda = None
            case OpKind.FN_RETURN:
                if self.return_types is None:
                    self.return_types = self.stack.copy()
                unified = _stacks_compatible(self.return_types, self.stack)
                if unified is None:
                    raise_error(
                        ErrorKind.TYPE_MISMATCH,
                        "Invalid return types",
                        op.location,
                        expected=str(self.return_types),
                        got=str(self.stack),
                    )
                self.return_types = unified
                if self.branched_stacks:
                    branched = self.branched_stacks[-1]
                    self.stack = branched.before.copy()
                    self.stack_origins = branched.before_origins.copy()
        return op_index

    def check_literals(self, op: Op, function: Function | None) -> None:
        """Handle literal push ops."""
        match op.kind:
            case OpKind.PUSH_INT:
                self.stack_push("int")
            case OpKind.PUSH_STR:
                self.stack_push("str")
            case OpKind.PUSH_BOOL:
                self.stack_push("bool")
            case OpKind.PUSH_CHAR:
                self.stack_push("char")
            case OpKind.PUSH_ARRAY:
                items = op.value
                assert isinstance(items, list)
                if not items:
                    self.stack_push("array[any]")
                    return
                first_type = _infer_literal_type(items[0], function)
                for item in items[1:]:
                    item_type = _infer_literal_type(item, function)
                    if item_type != first_type:
                        raise_error(
                            ErrorKind.TYPE_MISMATCH,
                            "Array literal has mixed element types",
                            op.location,
                            expected=f"`{first_type}`",
                            got=f"`{item_type}`",
                        )
                self.stack_push(f"array[{first_type}]")
            case OpKind.FSTRING_CONCAT:
                count = op.value
                assert isinstance(count, int)
                for _ in range(count):
                    self.expect_type("str")
                self.stack_push("str")

    def check_io(self, op: Op) -> None:
        """Handle PRINT."""
        typ = self.stack_pop()
        if typ == "str":
            op.kind = OpKind.PRINT_STR
        elif typ == "bool":
            op.kind = OpKind.PRINT_BOOL
        elif typ == "char":
            op.kind = OpKind.PRINT_CHAR
        elif typ == "cstr":
            op.kind = OpKind.PRINT_CSTR
        elif typ in GLOBAL_ENUMS:
            casa_enum = GLOBAL_ENUMS[typ]
            if casa_enum.has_inner_values:
                op.type_annotation = typ
            op.kind = OpKind.PRINT_INT
        else:
            enum_base = extract_generic_base(typ)
            if enum_base and enum_base in GLOBAL_ENUMS:
                casa_enum = GLOBAL_ENUMS[enum_base]
                if casa_enum.has_inner_values:
                    op.type_annotation = enum_base
            op.kind = OpKind.PRINT_INT

    def check_typeof(self, op: Op) -> None:
        """Handle TYPEOF."""
        typ = self.stack_pop()
        op.type_annotation = typ

    def check_syscalls(self, op: Op) -> None:
        """Handle SYSCALL0 through SYSCALL6."""
        arg_count = int(op.kind.name[-1])
        self.expect_type("int")
        for _ in range(arg_count):
            self.stack_pop()
        self.stack_push("int")

    def check_enum_variant(self, op: Op) -> None:
        """Handle PUSH_ENUM_VARIANT."""
        variant = op.value
        assert isinstance(variant, EnumVariant)
        assert variant.enum_name is not None
        casa_enum = GLOBAL_ENUMS[variant.enum_name]
        inner_types = casa_enum.variant_types.get(variant.variant_name, [])

        if not inner_types:
            # No inner values — just push enum type
            if casa_enum.type_vars:
                # Generic enum variant without data (e.g., Option::None)
                param_type = f"{casa_enum.name}[{' '.join(casa_enum.type_vars)}]"
                self.stack_push(param_type)
            else:
                self.stack_push(variant.enum_name)
            return

        # Data-carrying variant: pop inner values and push enum type
        if not casa_enum.type_vars:
            self.current_op_context = (
                f"`{variant.enum_name}::{variant.variant_name}`"
                f" ({' '.join(inner_types)} -> {variant.enum_name})"
            )
            for inner_type in inner_types:
                self.current_expect_context = (
                    f"inner value of `{variant.enum_name}::{variant.variant_name}`"
                )
                self.expect_type(inner_type)
            self.current_expect_context = None
            self.stack_push(variant.enum_name)
            return

        # Generic data-carrying variant: use apply_signature to bind type vars
        param_enum_type = f"{casa_enum.name}[{' '.join(casa_enum.type_vars)}]"
        params = [Parameter(t) for t in inner_types]
        sig = Signature(
            params,
            [param_enum_type],
            type_vars=set(casa_enum.type_vars),
        )
        self.apply_signature(sig, f"{variant.enum_name}::{variant.variant_name}")

    def check_is(self, op: Op, function: "Function | None" = None) -> None:
        """Handle IS_CHECK: pop enum, push bool, optionally bind inner values."""
        variant = op.value
        assert isinstance(variant, EnumVariant)
        assert variant.enum_name is not None

        # Pop the enum value
        typ = self.stack_pop()
        base_type = self._resolve_match_type(typ)

        # Validate the variant belongs to the correct enum
        if base_type not in GLOBAL_ENUMS:
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"`is` requires an enum type, got `{typ}`",
                op.location,
            )

        casa_enum = GLOBAL_ENUMS[base_type]
        if variant.enum_name != casa_enum.name:
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"Cannot check `{variant.enum_name}::{variant.variant_name}`"
                f" on value of type `{typ}`",
                op.location,
            )

        if variant.bindings:
            # Bindings only allowed in if/elif conditions
            if not self.in_branch_condition:
                raise_error(
                    ErrorKind.SYNTAX,
                    "`is` with bindings is only allowed in `if`/`elif` conditions",
                    op.location,
                )

            inner_types = casa_enum.variant_types.get(variant.variant_name, [])
            if len(variant.bindings) != len(inner_types):
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    f"Variant `{variant.enum_name}::{variant.variant_name}`"
                    f" has {len(inner_types)} inner value(s),"
                    f" but {len(variant.bindings)} binding(s) provided",
                    op.location,
                )

            # Resolve generic type vars from the concrete enum type
            type_bindings: dict[str, str] = {}
            if casa_enum.type_vars:
                params = extract_generic_params(typ)
                if params:
                    for tvar, concrete in zip(
                        casa_enum.type_vars, params, strict=False
                    ):
                        type_bindings[tvar] = concrete

            for var_name, inner_type in zip(variant.bindings, inner_types, strict=True):
                resolved_type = type_bindings.get(inner_type, inner_type)
                self._set_struct_binding_type(var_name, resolved_type, function)

        self.stack_push("bool")

    @staticmethod
    def _check_duplicate_arm(
        covered_key: str,
        display_name: str,
        location: Location,
        branched: BranchedStack,
    ) -> None:
        """Raise an error if a match arm has already been covered."""
        for label, _, _ in branched.branch_records:
            if label == covered_key:
                raise_error(
                    ErrorKind.DUPLICATE_NAME,
                    f"Duplicate match arm for {display_name}",
                    location,
                )
        if branched.current_branch_label == covered_key:
            raise_error(
                ErrorKind.DUPLICATE_NAME,
                f"Duplicate match arm for {display_name}",
                location,
            )

    @staticmethod
    def _check_literal_exhaustiveness(
        typ: str,
        covered: set[str],
        has_wildcard: bool,
        location: Location,
    ) -> None:
        """Check exhaustiveness for literal type matches."""
        if has_wildcard:
            return
        if typ == "bool":
            missing = [v for v in ("true", "false") if v not in covered]
            if missing:
                names = ", ".join(f"`{v}`" for v in missing)
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    f"Non-exhaustive match, missing arms: {names}",
                    location,
                )
            return
        # int, char, str have infinite/large domains — wildcard required
        raise_error(
            ErrorKind.TYPE_MISMATCH,
            f"Non-exhaustive match on type `{typ}`." f" A wildcard `_` arm is required",
            location,
        )

    def _check_match_arm_pattern(
        self,
        pattern: "EnumVariant | StructPattern | LiteralPattern",
        match_type: str,
        location: Location,
        branched: BranchedStack,
    ) -> str:
        """Validate a match arm pattern and return its covered_key."""
        if isinstance(pattern, LiteralPattern):
            if pattern.typ != match_type:
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    f"Literal `{pattern.raw}` has type `{pattern.typ}`,"
                    f" but match subject has type `{match_type}`",
                    location,
                )
            covered_key = f"__covered:{pattern.raw}"
            self._check_duplicate_arm(
                covered_key, f"`{pattern.raw}`", location, branched
            )
            return covered_key

        if isinstance(pattern, StructPattern):
            if pattern.struct_name != match_type:
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    f"Struct pattern `{pattern.struct_name}`"
                    f" does not match type `{match_type}`",
                    location,
                )
            covered_key = f"__covered:{pattern.struct_name}"
            self._check_duplicate_arm(
                covered_key, f"`{pattern.struct_name}`", location, branched
            )
            return covered_key

        # EnumVariant pattern
        variant = pattern
        if variant.is_wildcard:
            return f"__covered:{MATCH_WILDCARD}"

        base_match_type = self._resolve_match_type(match_type)

        if base_match_type in LITERAL_MATCH_TYPES:
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"Expected `{base_match_type}` literal or `_`,"
                f" got `{variant.variant_name}`",
                location,
            )

        if variant.enum_name is None:
            # Bare variant name without enum qualifier
            casa_enum = GLOBAL_ENUMS[base_match_type]
            assert casa_enum.variants
            if variant.variant_name in casa_enum.variants:
                raise_error(
                    ErrorKind.TYPE_MISMATCH,
                    f"Unqualified enum variant `{variant.variant_name}`."
                    f" Did you mean `{base_match_type}::{variant.variant_name}`?",
                    location,
                )
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"Expected qualified enum variant"
                f" (e.g. `{base_match_type}::{casa_enum.variants[0]}`) or `_`,"
                f" got `{variant.variant_name}`",
                location,
            )

        if variant.enum_name != base_match_type:
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"Match arm variant `{variant.enum_name}::{variant.variant_name}`"
                f" does not belong to enum `{base_match_type}`",
                location,
            )

        covered_key = f"__covered:{variant.variant_name}"
        self._check_duplicate_arm(
            covered_key,
            f"`{variant.enum_name}::{variant.variant_name}`",
            location,
            branched,
        )
        return covered_key

    @staticmethod
    def _set_struct_binding_type(
        var_name: str, field_type: Type, function: "Function | None"
    ) -> None:
        """Set the type on a variable bound in a struct match pattern."""
        if function:
            for variable in function.variables:
                if variable.name == var_name:
                    variable.typ = field_type
                    return
        global_var = GLOBAL_VARIABLES.get(var_name)
        if global_var:
            global_var.typ = field_type

    @staticmethod
    def _resolve_match_type(typ: str) -> str:
        """Resolve a match type to the base enum/struct name for lookups."""
        if typ in GLOBAL_ENUMS or typ in GLOBAL_STRUCTS:
            return typ
        base = extract_generic_base(typ)
        if base and base in GLOBAL_ENUMS:
            return base
        return typ

    def check_match(self, op: Op, function: "Function | None" = None) -> None:
        """Handle MATCH_START, MATCH_ARM, MATCH_END."""
        match op.kind:
            case OpKind.MATCH_START:
                typ = self.stack_pop()
                base = self._resolve_match_type(typ)
                if (
                    base not in GLOBAL_ENUMS
                    and base not in GLOBAL_STRUCTS
                    and base not in LITERAL_MATCH_TYPES
                ):
                    raise_error(
                        ErrorKind.TYPE_MISMATCH,
                        f"Match requires an enum, struct, or literal type,"
                        f" got `{typ}`",
                        op.location,
                    )
                bs = BranchedStack(self.stack, self.stack_origins)
                bs.if_location = op.location
                bs.if_location_match_type = typ
                bs.default_present = True
                self.branched_stacks.append(bs)
            case OpKind.MATCH_ARM:
                pattern = op.value
                assert isinstance(pattern, (EnumVariant, StructPattern, LiteralPattern))
                assert self.branched_stacks, "Match block stack state is saved"
                branched = self.branched_stacks[-1]

                match_type = branched.if_location_match_type
                assert match_type is not None

                # Check for arms after wildcard
                wildcard_key = f"__covered:{MATCH_WILDCARD}"
                has_wildcard = (
                    any(
                        label == wildcard_key for label, _, _ in branched.branch_records
                    )
                    or branched.current_branch_label == wildcard_key
                )

                if has_wildcard:
                    raise_error(
                        ErrorKind.SYNTAX,
                        "Match arms after wildcard `_` are unreachable",
                        op.location,
                    )

                covered_key = self._check_match_arm_pattern(
                    pattern,
                    match_type,
                    op.location,
                    branched,
                )

                # Handle branch state like elif
                if branched.condition_present:
                    # Record the PREVIOUS arm's stack state
                    if branched.current_branch_location:
                        prev_covered = branched.current_branch_label
                        if prev_covered and prev_covered.startswith("__covered:"):
                            branched.branch_records.append(
                                (
                                    prev_covered,
                                    self.stack.copy(),
                                    branched.current_branch_location,
                                )
                            )
                    if self.stack == branched.before == branched.after:
                        pass
                    else:
                        unified = _stacks_compatible(self.stack, branched.after)
                        if unified is not None and branched.before != branched.after:
                            branched.after = unified
                        elif branched.before == branched.after:
                            branched.after = self.stack.copy()
                            branched.after_origins = self.stack_origins.copy()
                        else:
                            raise_error(
                                ErrorKind.STACK_MISMATCH,
                                "Match arms have incompatible stack effects",
                                op.location,
                                notes=_branch_mismatch_notes(branched),
                            )
                    self.stack = branched.before.copy()
                    self.stack_origins = branched.before_origins.copy()
                else:
                    branched.condition_present = True
                    branched.before = self.stack.copy()
                    branched.before_origins = self.stack_origins.copy()
                    branched.after = branched.before
                    branched.after_origins = branched.before_origins

                # For struct patterns, set types on bound variables
                if isinstance(pattern, StructPattern):
                    struct = GLOBAL_STRUCTS[pattern.struct_name]
                    member_type_by_name = {m.name: m.typ for m in struct.members}
                    for field_name, var_name in pattern.bindings.items():
                        field_type = member_type_by_name[field_name]
                        self._set_struct_binding_type(var_name, field_type, function)

                # For enum variant patterns with bindings, set types
                if isinstance(pattern, EnumVariant) and pattern.bindings:
                    base_type = self._resolve_match_type(match_type)
                    casa_enum = GLOBAL_ENUMS[base_type]
                    inner_types = casa_enum.variant_types.get(pattern.variant_name, [])
                    if len(pattern.bindings) != len(inner_types):
                        raise_error(
                            ErrorKind.TYPE_MISMATCH,
                            f"Variant `{pattern.enum_name}::{pattern.variant_name}`"
                            f" has {len(inner_types)} inner value(s),"
                            f" but {len(pattern.bindings)} binding(s) provided",
                            op.location,
                        )
                    # Resolve generic type vars from match subject type
                    type_bindings: dict[str, str] = {}
                    if casa_enum.type_vars:
                        params = extract_generic_params(match_type)
                        if params:
                            for tvar, concrete in zip(
                                casa_enum.type_vars, params, strict=False
                            ):
                                type_bindings[tvar] = concrete
                    for var_name, inner_type in zip(
                        pattern.bindings, inner_types, strict=True
                    ):
                        resolved_type = type_bindings.get(inner_type, inner_type)
                        self._set_struct_binding_type(var_name, resolved_type, function)

                branched.current_branch_label = covered_key
                branched.current_branch_location = op.location

            case OpKind.MATCH_END:
                assert self.branched_stacks, "Match block stack state is saved"
                branched = self.branched_stacks.pop()

                # Record last arm
                if (
                    branched.current_branch_label
                    and branched.current_branch_label.startswith("__covered:")
                    and branched.current_branch_location
                ):
                    branched.branch_records.append(
                        (
                            branched.current_branch_label,
                            self.stack.copy(),
                            branched.current_branch_location,
                        )
                    )

                # Extract matched type from if_location metadata
                match_type = branched.if_location_match_type
                assert match_type is not None
                covered = {
                    label.split(":", 1)[1]
                    for label, _, _ in branched.branch_records
                    if label.startswith("__covered:")
                }
                has_wildcard = MATCH_WILDCARD in covered

                base_match_type = self._resolve_match_type(match_type)

                if base_match_type in GLOBAL_STRUCTS:
                    # Struct match: one struct arm or wildcard is exhaustive
                    if not has_wildcard and base_match_type not in covered:
                        raise_error(
                            ErrorKind.TYPE_MISMATCH,
                            f"Non-exhaustive match on struct `{match_type}`",
                            op.location,
                        )
                elif base_match_type in LITERAL_MATCH_TYPES:
                    self._check_literal_exhaustiveness(
                        base_match_type, covered, has_wildcard, op.location
                    )
                else:
                    casa_enum = GLOBAL_ENUMS[base_match_type]
                    if not has_wildcard:
                        missing = [v for v in casa_enum.variants if v not in covered]
                        if missing:
                            names = ", ".join(
                                f"`{base_match_type}::{v}`" for v in missing
                            )
                            raise_error(
                                ErrorKind.TYPE_MISMATCH,
                                f"Non-exhaustive match, missing arms: {names}",
                                op.location,
                            )

                # Apply final stack state
                # Identity check: after is the same object as before when only
                # one arm ran (wildcard-only match), so accept current stack
                if branched.before is branched.after:
                    pass
                elif (
                    unified := _stacks_compatible(self.stack, branched.after)
                ) is not None:
                    self.stack = unified
                    self.stack_origins = branched.after_origins.copy()
                elif self.stack == branched.before == branched.after:
                    pass
                else:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Match arms have incompatible stack effects",
                        op.location,
                        notes=_branch_mismatch_notes(branched),
                    )

    def check_struct_new(self, op: Op) -> None:
        """Handle STRUCT_NEW."""
        struct = op.value
        assert isinstance(struct, Struct), "Expected struct"
        member_types = " ".join(m.typ for m in struct.members)

        if not struct.type_vars:
            self.current_op_context = (
                f"`{struct.name}` ({member_types} -> {struct.name})"
            )
            for member in struct.members:
                self.current_expect_context = (
                    f"member `{member.name}` of `{struct.name}`"
                )
                self.expect_type(member.typ)
            self.current_expect_context = None
            self.stack_push(struct.name)
            return

        # Generic struct: bind type vars from actual stack types
        param_struct_type = f"{struct.name}[{' '.join(struct.type_vars)}]"
        params = [Parameter(m.typ) for m in struct.members]
        sig = Signature(
            params,
            [param_struct_type],
            type_vars=set(struct.type_vars),
        )
        self.apply_signature(sig, struct.name)

    def check_struct_literal(self, op: Op) -> None:
        """Handle STRUCT_LITERAL."""
        literal = op.value
        assert isinstance(literal, StructLiteral)
        struct = literal.struct

        if not struct.type_vars:
            member_types = " ".join(m.typ for m in struct.members)
            self.current_op_context = (
                f"`{struct.name}` ({member_types} -> {struct.name})"
            )
            # Pop field values in reverse parse order (last pushed is on top)
            member_type_by_name = {m.name: m.typ for m in struct.members}
            for field_name in reversed(literal.field_order):
                self.current_expect_context = f"field `{field_name}` of `{struct.name}`"
                self.expect_type(member_type_by_name[field_name])
            self.current_expect_context = None
            self.stack_push(struct.name)
            return

        # Generic struct: build a signature with fields in parse order
        member_type_by_name = {m.name: m.typ for m in struct.members}
        params = [Parameter(member_type_by_name[name]) for name in literal.field_order]
        param_struct_type = f"{struct.name}[{' '.join(struct.type_vars)}]"
        sig = Signature(
            params,
            [param_struct_type],
            type_vars=set(struct.type_vars),
        )
        self.apply_signature(sig, struct.name)

    DEFERRED_METHOD = "DEFERRED"

    @staticmethod
    def _find_method_by_name(
        method_name: str, location: Location
    ) -> tuple[Function | None, str, str | None]:
        """Search all global functions for a method matching ::method_name.

        Used when the receiver type is 'any' (e.g. inside lambdas where the
        stack type is unknown). Returns (function, name, deferred_return_type).
        When exactly one match is found, returns it directly. When multiple
        matches share a common return type, returns a deferred sentinel.
        """
        suffix = f"::{method_name}"
        matches: list[tuple[str, Function]] = []
        for name, func in GLOBAL_FUNCTIONS.items():
            if name.endswith(suffix) and not name.startswith("lambda__"):
                matches.append((name, func))
        if len(matches) == 1:
            return matches[0][1], matches[0][0], None
        if len(matches) > 1:
            return _defer_ambiguous_method(matches, method_name, location)
        return None, f"any::{method_name}", None

    def check_method_call(
        self,
        op: Op,
        ops: list[Op],
        op_index: int,
        function: Function | None,
    ) -> int:
        """Handle METHOD_CALL. Returns updated op_index."""
        method_name = op.value
        assert isinstance(method_name, str), "Expected method name"
        receiver = self.stack_peek()

        if (
            function
            and function.signature
            and function.signature.trait_bounds
            and receiver in function.signature.trait_bounds
        ):
            trait_name = function.signature.trait_bounds[receiver]
            trait = GLOBAL_TRAITS.get(trait_name)
            if trait:
                for trait_method in trait.methods:
                    if trait_method.name != method_name:
                        continue
                    hidden_var = f"__trait_{receiver}_{method_name}"
                    self.stack_pop()
                    resolved_sig = _resolve_trait_sig(trait_method.signature, receiver)
                    self.stack_push(receiver)
                    self.apply_signature(resolved_sig, f"{receiver}::{method_name}")
                    op.kind = OpKind.PUSH_VARIABLE
                    op.value = hidden_var
                    exec_op = Op(
                        Intrinsic.EXEC,
                        OpKind.FN_EXEC,
                        op.location,
                    )
                    ops.insert(op_index, exec_op)
                    op_index += 1
                    return op_index
                raise_error(
                    ErrorKind.UNDEFINED_NAME,
                    f"Method `{method_name}` not found in trait `{trait_name}`",
                    op.location,
                )

        function_name = f"{receiver}::{method_name}"
        global_function = GLOBAL_FUNCTIONS.get(function_name)
        receiver_base = extract_generic_base(receiver)
        if not global_function and receiver_base:
            function_name = f"{receiver_base}::{method_name}"
            global_function = GLOBAL_FUNCTIONS.get(function_name)
        if not global_function and receiver == ANY_TYPE:
            global_function, function_name, deferred_ret = self._find_method_by_name(
                method_name, op.location
            )
            if not global_function and function_name == self.DEFERRED_METHOD:
                # Deferred method: receiver is any, multiple candidates with
                # common return type. Leave as METHOD_CALL for specialization.
                assert deferred_ret is not None
                op.deferred_return_type = deferred_ret
                self.stack_pop()
                self.stack_push(deferred_ret)
                if function:
                    function.has_deferred_methods = True
                return op_index
        if not global_function:
            raise_error(
                ErrorKind.UNDEFINED_NAME,
                f"Method `{function_name}` does not exist",
                op.location,
            )

        if not global_function.is_typechecked:
            global_function.ops = resolve_identifiers(
                global_function.ops, global_function
            )
            global_function.is_used = True
        _ensure_typechecked(global_function)

        assert global_function.signature, "Signature is defined"
        fn_sig = global_function.signature

        if fn_sig.trait_bounds:
            injected = _inject_trait_fn_ptrs(
                self, fn_sig, ops, op_index, op.location, function
            )
            op_index += injected

        self.apply_signature(fn_sig, function_name)
        op.value = function_name
        op.kind = OpKind.FN_CALL
        return op_index


def _resolve_return_type(return_type: Type, bindings: dict[str, Type]) -> Type:
    """Resolve a return type by substituting bound type variables."""
    if return_type in bindings:
        return bindings[return_type]
    generic_base = extract_generic_base(return_type)
    if generic_base:
        ret_params = extract_generic_params(return_type)
        if ret_params and any(p in bindings for p in ret_params):
            resolved = [bindings.get(p, p) for p in ret_params]
            return f"{generic_base}[{' '.join(resolved)}]"
    fn_sig_str = extract_fn_signature_str(return_type)
    if fn_sig_str and any(type_var in fn_sig_str for type_var in bindings):
        resolved_sig = fn_sig_str
        for type_var, bound in bindings.items():
            resolved_sig = resolved_sig.replace(type_var, bound)
        return f"fn[{resolved_sig}]"
    return return_type


def _check_passthrough_params(
    declared: Signature, inferred: Signature
) -> list[Parameter] | None:
    """Check if the mismatch is explained by unused passthrough parameters.

    Unnamed declared parameters stay on the data stack untouched. The body
    operates above them, so the effective return types are the passthrough
    parameter types (bottom of stack) plus the inferred return types (top).

    Returns the list of unused parameters if valid, or None if it is a
    genuine mismatch.
    """
    n_declared = len(declared.parameters)
    n_inferred = len(inferred.parameters)

    if n_inferred > n_declared:
        return None

    # The body pops from the top of the stack. First-declared params are on
    # top, so the first n_inferred declared params must match the inferred.
    for d, i in zip(declared.parameters[:n_inferred], inferred.parameters):
        if d.typ != i.typ and d.typ != ANY_TYPE and i.typ != ANY_TYPE:
            return None

    # Unused params sit at the bottom. In stack order (bottom-to-top) they
    # are the last n_unused declared params reversed.
    unused = declared.parameters[n_inferred:]
    passthrough_types = [p.typ for p in reversed(unused)]

    # Effective returns = passthrough (bottom) + inferred returns (top)
    effective_returns = passthrough_types + inferred.return_types
    if len(effective_returns) != len(declared.return_types):
        return None

    for eff, dec in zip(effective_returns, declared.return_types):
        if eff != dec and eff != ANY_TYPE and dec != ANY_TYPE:
            return None

    return unused


OP_STACK_EFFECTS: dict[OpKind, tuple[str, str]] = {
    OpKind.ADD: ("+", "int int -> int"),
    OpKind.SUB: ("-", "int int -> int"),
    OpKind.MUL: ("*", "int int -> int"),
    OpKind.DIV: ("/", "int int -> int"),
    OpKind.MOD: ("%", "int int -> int"),
    OpKind.SHL: ("<<", "int int -> int"),
    OpKind.SHR: (">>", "int int -> int"),
    OpKind.BIT_AND: ("&", "int int -> int"),
    OpKind.BIT_OR: ("|", "int int -> int"),
    OpKind.BIT_XOR: ("^", "int int -> int"),
    OpKind.BIT_NOT: ("~", "int -> int"),
    OpKind.NOT: ("!", "bool -> bool"),
    OpKind.EQ: ("==", "any any -> bool"),
    OpKind.NE: ("!=", "any any -> bool"),
    OpKind.LT: ("<", "any any -> bool"),
    OpKind.LE: ("<=", "any any -> bool"),
    OpKind.GT: (">", "any any -> bool"),
    OpKind.GE: (">=", "any any -> bool"),
    OpKind.AND: ("&&", "bool bool -> bool"),
    OpKind.OR: ("||", "bool bool -> bool"),
    OpKind.DROP: ("drop", "any -> None"),
    OpKind.DUP: ("dup", "any -> any any"),
    OpKind.SWAP: ("swap", "any any -> any any"),
    OpKind.OVER: ("over", "any any -> any any any"),
    OpKind.ROT: ("rot", "any any any -> any any any"),
    OpKind.PRINT: ("print", "any -> None"),
    OpKind.PRINT_INT: ("print", "any -> None"),
    OpKind.PRINT_STR: ("print", "any -> None"),
    OpKind.PRINT_BOOL: ("print", "any -> None"),
    OpKind.PRINT_CHAR: ("print", "any -> None"),
    OpKind.PRINT_CSTR: ("print", "any -> None"),
    OpKind.FN_EXEC: ("exec", "fn[sig] -> ..."),
    OpKind.TYPEOF: ("typeof", "any -> str"),
    OpKind.ASSIGN_DECREMENT: ("-=", "int -> None"),
    OpKind.ASSIGN_INCREMENT: ("+=", "int -> None"),
    OpKind.HEAP_ALLOC: ("alloc", "int -> ptr"),
    OpKind.LOAD8: ("load8", "ptr -> int"),
    OpKind.LOAD16: ("load16", "ptr -> int"),
    OpKind.LOAD32: ("load32", "ptr -> int"),
    OpKind.LOAD64: ("load64", "ptr -> int"),
    OpKind.STORE8: ("store8", "any ptr -> None"),
    OpKind.STORE16: ("store16", "any ptr -> None"),
    OpKind.STORE32: ("store32", "any ptr -> None"),
    OpKind.STORE64: ("store64", "any ptr -> None"),
    OpKind.IF_CONDITION: ("if condition", "bool -> None"),
    OpKind.WHILE_CONDITION: ("while condition", "bool -> None"),
    OpKind.SYSCALL0: ("syscall0", "int -> int"),
    OpKind.SYSCALL1: ("syscall1", "any int -> int"),
    OpKind.SYSCALL2: ("syscall2", "any any int -> int"),
    OpKind.SYSCALL3: ("syscall3", "any any any int -> int"),
    OpKind.SYSCALL4: ("syscall4", "any any any any int -> int"),
    OpKind.SYSCALL5: ("syscall5", "any any any any any int -> int"),
    OpKind.SYSCALL6: ("syscall6", "any any any any any any int -> int"),
    OpKind.ARGC: ("argc", "None -> int"),
    OpKind.ARGV: ("argv", "None -> ptr"),
}


def _infer_literal_type(op: Op, function: Function | None = None) -> str:
    """Infer the type of an array item Op."""
    match op.kind:
        case OpKind.PUSH_CHAR:
            return "char"
        case OpKind.PUSH_INT:
            return "int"
        case OpKind.PUSH_STR:
            return "str"
        case OpKind.PUSH_BOOL:
            return "bool"
        case OpKind.PUSH_ARRAY:
            items = op.value
            if not items:
                return "array[any]"
            return f"array[{_infer_literal_type(items[0], function)}]"
        case OpKind.PUSH_VARIABLE:
            var_name = op.value
            assert isinstance(var_name, str)
            if function:
                for var in function.variables:
                    if var.name == var_name and var.typ:
                        return var.typ
            gvar = GLOBAL_VARIABLES.get(var_name)
            if gvar and gvar.typ:
                return gvar.typ
            return ANY_TYPE
        case OpKind.PUSH_CAPTURE:
            cap_name = op.value
            assert isinstance(cap_name, str)
            if function:
                for cap in function.captures:
                    if cap.name == cap_name and cap.typ:
                        return cap.typ
            return ANY_TYPE
        case _:
            return ANY_TYPE


def _format_branch_signature(before: list[Type], after: list[Type]) -> str:
    """Format a branch's stack effect as 'type1 type2 -> type3'."""
    before_str = " ".join(before) if before else "None"
    after_str = " ".join(after) if after else "None"
    return f"`{before_str} -> {after_str}`"


def _is_enum_type_var(typ: Type) -> bool:
    """Check if a type is an unresolved enum type variable (e.g. T, E).

    Only matches type variables that don't shadow a concrete type (struct/enum).
    """
    if typ in GLOBAL_STRUCTS or typ in GLOBAL_ENUMS:
        return False
    for casa_enum in GLOBAL_ENUMS.values():
        if typ in casa_enum.type_vars:
            return True
    return False


def _unify_type(a: Type, b: Type) -> Type | None:
    """Resolve two compatible types to the more specific one.

    Returns the unified type, or None if the types are incompatible.
    """
    if a == b:
        return a
    if a == ANY_TYPE or _is_enum_type_var(a):
        return b
    if b == ANY_TYPE or _is_enum_type_var(b):
        return a
    base_a = extract_generic_base(a)
    base_b = extract_generic_base(b)
    if base_a is not None and base_a == base_b:
        params_a = extract_generic_params(a)
        params_b = extract_generic_params(b)
        if params_a is None:
            return b
        if params_b is None:
            return a
        if len(params_a) != len(params_b):
            return None
        unified_params = []
        for pa, pb in zip(params_a, params_b):
            u = _unify_type(pa, pb)
            if u is None:
                return None
            unified_params.append(u)
        return f"{base_a}[{' '.join(unified_params)}]"
    if base_a is not None and base_a == b:
        return a
    if base_b is not None and base_b == a:
        return b
    return None


def _stacks_compatible(stack_a: list[Type], stack_b: list[Type]) -> list[Type] | None:
    """Compare two stacks element-wise using type compatibility rules.

    Returns the unified (more specific) stack if compatible, None otherwise.
    """
    if len(stack_a) != len(stack_b):
        return None
    unified: list[Type] = []
    for a, b in zip(stack_a, stack_b):
        result = _unify_type(a, b)
        if result is None:
            return None
        unified.append(result)
    return unified


def _branch_mismatch_notes(
    branched: BranchedStack,
) -> list[tuple[str, Location]]:
    """Build note entries from recorded branch results."""
    notes: list[tuple[str, Location]] = []
    for label, stack_after, location in branched.branch_records:
        sig = _format_branch_signature(branched.before, stack_after)
        notes.append((f"`{label}` branch has signature {sig}", location))
    return notes


def _ensure_typechecked(global_function: Function) -> None:
    """Type-check a function if not already done, preserving declared signatures."""
    if global_function.is_typechecked:
        return
    global_function.is_typechecked = True
    signature = type_check_ops(global_function.ops, global_function)
    if not global_function.signature:
        global_function.signature = signature


def _defer_ambiguous_method(
    matches: list[tuple[str, Function]],
    method_name: str,
    location: Location,
) -> tuple[Function | None, str, str | None]:
    """Handle ambiguous method lookup by checking for a common return type.

    If all candidates share the same return type, the method is deferred for
    monomorphization at each call site. Returns (None, DEFERRED, return_type).
    Otherwise, reports an ambiguity error.
    """
    for _, func in matches:
        if not func.is_typechecked:
            func.ops = resolve_identifiers(func.ops, func)
            func.is_used = True
        _ensure_typechecked(func)

    return_types: set[str] = set()
    for _, func in matches:
        if func.signature:
            for ret in func.signature.return_types:
                return_types.add(ret)

    if len(return_types) == 1:
        common_ret = return_types.pop()
        return None, TypeChecker.DEFERRED_METHOD, common_ret

    candidates = ", ".join(name for name, _ in matches)
    raise_error(
        ErrorKind.UNDEFINED_NAME,
        f"Ambiguous method `.{method_name}`, candidates: {candidates}",
        location,
    )
    return None, f"any::{method_name}", None


def _try_specialize_deferred_exec(
    tc: TypeChecker,
    fn_ptr: str,
    ops: list[Op],
    op_index: int,
    exec_op: Op,
) -> str | None:
    """Specialize a deferred method lambda at an exec call site.

    When a lambda has deferred (unresolved) methods and is called via exec,
    this creates a monomorphized clone with the method resolved for the
    concrete argument type. Returns the specialized fn type, or None.
    """
    if not isinstance(fn_ptr, str) or not fn_ptr.startswith("fn["):
        return None
    if ANY_TYPE not in fn_ptr:
        return None

    # Find the source lambda
    source_lambda_name = _find_deferred_lambda_source(tc, ops, op_index)
    if not source_lambda_name:
        return None

    source_lambda = GLOBAL_FUNCTIONS.get(source_lambda_name)
    if not source_lambda or not source_lambda.has_deferred_methods:
        return None

    # Determine the concrete argument type from the stack
    # Stack: [..., arg, fn_ptr]. fn_ptr is at top, arg is below.
    if len(tc.stack) < 2:
        return None
    concrete_type = tc.stack[-2]
    if concrete_type == ANY_TYPE:
        return None

    # Create or reuse a specialized clone
    clone_name = f"{source_lambda_name}__spec_{concrete_type}"
    existing = GLOBAL_FUNCTIONS.get(clone_name)
    if not existing:
        clone = _clone_and_specialize_lambda(source_lambda, clone_name, concrete_type)
        if not clone:
            return None
        GLOBAL_FUNCTIONS[clone_name] = clone
        _ensure_typechecked(clone)
        existing = clone

    # Verify the clone's parameter type matches the concrete argument type
    if existing.signature and existing.signature.parameters:
        expected_param = existing.signature.parameters[0].typ
        if (
            expected_param != concrete_type
            and expected_param != ANY_TYPE
            and concrete_type != ANY_TYPE
        ):
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                "Type mismatch in deferred method call",
                exec_op.location,
                expected=f"`{expected_param}`",
                got=f"`{concrete_type}`",
            )

    # Replace the push op to push the clone directly
    prev_op_idx = op_index - 2
    if prev_op_idx >= 0:
        prev_op = ops[prev_op_idx]
        if prev_op.kind in (OpKind.PUSH_VARIABLE, OpKind.FN_PUSH, OpKind.PUSH_CAPTURE):
            prev_op.kind = OpKind.FN_PUSH
            prev_op.value = clone_name

    # The original deferred lambda should not be compiled
    source_lambda.is_used = False

    # Update the stack so the concrete fn type is used by apply_signature
    concrete_fn_type = f"fn[{existing.signature}]"
    tc.stack[-1] = concrete_fn_type

    return concrete_fn_type


def _find_deferred_lambda_source(
    tc: TypeChecker, ops: list[Op], op_index: int
) -> str | None:
    """Find the source lambda name for the fn pointer about to be exec'd."""
    prev_op_idx = op_index - 2
    if prev_op_idx < 0:
        return None
    prev_op = ops[prev_op_idx]
    if prev_op.kind == OpKind.FN_PUSH:
        return prev_op.value if isinstance(prev_op.value, str) else None
    if prev_op.kind in (OpKind.PUSH_VARIABLE, OpKind.PUSH_CAPTURE):
        var_name = prev_op.value
        if isinstance(var_name, str):
            return tc.deferred_lambda_vars.get(var_name)
    return None


def _clone_and_specialize_lambda(
    source: Function, clone_name: str, concrete_type: str
) -> Function | None:
    """Clone a lambda and resolve deferred METHOD_CALL ops for concrete_type."""
    import copy

    cloned_ops = copy.deepcopy(source.ops)
    for cloned_op in cloned_ops:
        if cloned_op.kind != OpKind.METHOD_CALL:
            continue
        if cloned_op.deferred_return_type is None:
            continue
        method_name = cloned_op.value
        assert isinstance(method_name, str)
        # Resolve the method for the concrete type
        fn_name = f"{concrete_type}::{method_name}"
        resolved = GLOBAL_FUNCTIONS.get(fn_name)
        base = extract_generic_base(concrete_type)
        if not resolved and base:
            fn_name = f"{base}::{method_name}"
            resolved = GLOBAL_FUNCTIONS.get(fn_name)
        if not resolved:
            return None
        cloned_op.kind = OpKind.FN_CALL
        cloned_op.value = fn_name
        cloned_op.deferred_return_type = None

    clone = Function(clone_name, cloned_ops, source.location)
    clone.is_used = True
    return clone


def _resolve_trait_sig(sig: Signature, type_var: str) -> Signature:
    """Create a copy of a trait method signature with self type replaced by type_var."""
    return resolve_trait_sig(sig, type_var)


def _bind_generic_params(
    bindings: dict[str, str],
    expected_type: str,
    actual_type: str,
    type_vars: set[str],
) -> None:
    """Bind type variables from a parameterized type match.

    Given expected 'Map[K V]' and actual 'Map[str int]', binds K=str, V=int.
    """
    exp_base = extract_generic_base(expected_type)
    if not exp_base:
        return
    act_base = extract_generic_base(actual_type)
    if act_base != exp_base:
        return
    exp_params = extract_generic_params(expected_type)
    act_params = extract_generic_params(actual_type)
    if not exp_params or not act_params:
        return
    for expected_param, actual_param in zip(exp_params, act_params):
        if expected_param in type_vars and expected_param not in bindings:
            bindings[expected_param] = actual_param


def _inject_trait_fn_ptrs(
    tc: "TypeChecker",
    sig: Signature,
    ops: list[Op],
    op_index: int,
    location: Location,
    function: Function | None,
) -> int:
    """Auto-inject FN_PUSH ops for trait-bounded function calls.

    Determines concrete types for type variables, checks structural trait
    satisfaction, and inserts FN_PUSH ops before the call. Returns the
    number of ops inserted.
    """
    from casa.parser import (  # pylint: disable=reimported,import-outside-toplevel
        resolve_identifiers as _resolve,
    )

    bindings: dict[str, str] = {}

    # Non-hidden parameters (user-visible)
    non_hidden = [
        p for p in sig.parameters if not p.name or not p.name.startswith("__trait_")
    ]

    # Bind type vars from the stack (peek at stack positions)
    stack_depth = len(tc.stack)
    for i, p in enumerate(non_hidden):
        stack_idx = stack_depth - 1 - i
        if stack_idx < 0:
            break
        actual = tc.stack[stack_idx]
        if p.typ in sig.type_vars:
            if p.typ not in bindings:
                bindings[p.typ] = actual
            continue
        _bind_generic_params(bindings, p.typ, actual, sig.type_vars)

    # Also check TYPE_CAST look-ahead for return type binding
    if op_index < len(ops) and ops[op_index].kind == OpKind.TYPE_CAST:
        cast_type = ops[op_index].value
        assert isinstance(cast_type, str)
        for ret in sig.return_types:
            _bind_generic_params(bindings, ret, cast_type, sig.type_vars)

    # Inject FN_PUSH ops for each trait bound's methods.
    # Methods are pushed in reverse order so the first method ends up on top
    # (first declared param = top of stack).
    inserted = 0
    insert_pos = op_index - 1  # Insert before the FN_CALL
    for tv, trait_name in sig.trait_bounds.items():
        concrete_type = bindings.get(tv)
        if not concrete_type:
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"Cannot determine concrete type for type variable `{tv}`",
                location,
            )
        trait = GLOBAL_TRAITS[trait_name]
        # For type variables, forward the hidden fn ptrs
        is_forwarding = (
            function
            and function.signature
            and function.signature.trait_bounds.get(concrete_type) == trait_name
        )
        if is_forwarding:
            for method in reversed(trait.methods):
                hidden_var = f"__trait_{concrete_type}_{method.name}"
                push_op = Op(hidden_var, OpKind.PUSH_VARIABLE, location)
                ops.insert(insert_pos, push_op)
                insert_pos += 1
                inserted += 1
                fn_sig = _resolve_trait_sig(method.signature, concrete_type)
                tc.stack_push(f"fn[{fn_sig}]")
            continue
        # Verify structural satisfaction
        if not type_satisfies_trait(concrete_type, trait_name, function):
            raise_error(
                ErrorKind.MISSING_TRAIT_METHOD,
                f"Type `{concrete_type}` does not satisfy trait `{trait_name}`",
                location,
            )
        for method in reversed(trait.methods):
            fn_name = f"{concrete_type}::{method.name}"
            global_fn = GLOBAL_FUNCTIONS.get(fn_name)
            if not global_fn or not global_fn.signature:
                raise_error(
                    ErrorKind.MISSING_TRAIT_METHOD,
                    f"Function `{fn_name}` required by trait `{trait_name}` not found",
                    location,
                )
            if not global_fn.is_used:
                global_fn.is_used = True
                global_fn.ops = _resolve(global_fn.ops, global_fn)
            _ensure_typechecked(global_fn)
            push_op = Op(fn_name, OpKind.FN_PUSH, location)
            ops.insert(insert_pos, push_op)
            insert_pos += 1
            inserted += 1
            tc.stack_push(f"fn[{global_fn.signature}]")
    return inserted


def type_satisfies_trait(
    type_name: str,
    trait_name: str,
    function: Function | None = None,
) -> bool:
    """Check if a type structurally satisfies a trait.

    A type satisfies a trait when its impl block contains all required methods
    with matching signatures (self type replaced by the concrete type).
    For type variables, checks if the type var has a matching trait bound.
    """
    trait = GLOBAL_TRAITS.get(trait_name)
    if not trait:
        return False
    # Type variable with matching bound satisfies the trait
    if function and function.signature and function.signature.trait_bounds:
        bound = function.signature.trait_bounds.get(type_name)
        if bound == trait_name:
            return True
    for method in trait.methods:
        fn_name = f"{type_name}::{method.name}"
        trait_fn = GLOBAL_FUNCTIONS.get(fn_name)
        if not trait_fn:
            return False
        if not trait_fn.signature:
            return False
        # Check signature matches with self type replaced by type_name
        resolved = resolve_trait_sig(method.signature, type_name)
        if len(trait_fn.signature.parameters) != len(resolved.parameters):
            return False
        if len(trait_fn.signature.return_types) != len(resolved.return_types):
            return False
        for actual_param, expected_param in zip(
            trait_fn.signature.parameters, resolved.parameters
        ):
            if (
                actual_param.typ != expected_param.typ
                and actual_param.typ != ANY_TYPE
                and expected_param.typ != ANY_TYPE
            ):
                return False
        for actual_ret, expected_ret in zip(
            trait_fn.signature.return_types, resolved.return_types
        ):
            if (
                actual_ret != expected_ret
                and actual_ret != ANY_TYPE
                and expected_ret != ANY_TYPE
            ):
                return False
    return True


def type_check_ops(ops: list[Op], function: Function | None = None) -> Signature:
    """Type-check a list of ops and return the inferred signature."""
    assert len(OpKind) == 86, "Exhaustive handling for `OpKind`"

    tc = TypeChecker(ops=ops)
    op_index = 0
    while op_index < len(ops):
        op = ops[op_index]
        op_index += 1
        tc.current_location = op.location
        tc.current_expect_context = None
        effect = OP_STACK_EFFECTS.get(op.kind)
        if effect:
            name, sig = effect
            tc.current_op_context = f"`{name}` ({sig})"
        else:
            tc.current_op_context = None
        match op.kind:
            case OpKind.ADD | OpKind.SUB | OpKind.DIV | OpKind.MOD | OpKind.MUL:
                tc.check_arithmetic(op)
            case OpKind.EQ | OpKind.NE | OpKind.LT | OpKind.LE | OpKind.GT | OpKind.GE:
                tc.check_comparison(op)
            case OpKind.AND | OpKind.OR | OpKind.NOT:
                tc.check_boolean(op)
            case (
                OpKind.BIT_AND
                | OpKind.BIT_OR
                | OpKind.BIT_XOR
                | OpKind.BIT_NOT
                | OpKind.SHL
                | OpKind.SHR
            ):
                tc.check_bitwise(op)
            case OpKind.DROP | OpKind.DUP | OpKind.SWAP | OpKind.OVER | OpKind.ROT:
                tc.check_stack_ops(op)
            case (
                OpKind.LOAD8
                | OpKind.LOAD16
                | OpKind.LOAD32
                | OpKind.LOAD64
                | OpKind.STORE8
                | OpKind.STORE16
                | OpKind.STORE32
                | OpKind.STORE64
                | OpKind.HEAP_ALLOC
            ):
                tc.check_memory(op)
            case (
                OpKind.ASSIGN_VARIABLE
                | OpKind.ASSIGN_INCREMENT
                | OpKind.ASSIGN_DECREMENT
            ):
                tc.check_assignment(op, function)
            case OpKind.PUSH_VARIABLE | OpKind.PUSH_CAPTURE:
                tc.check_push(op, function)
            case (
                OpKind.IF_START
                | OpKind.IF_CONDITION
                | OpKind.IF_ELIF
                | OpKind.IF_ELSE
                | OpKind.IF_END
            ):
                tc.check_if_block(op)
            case (
                OpKind.WHILE_START
                | OpKind.WHILE_CONDITION
                | OpKind.WHILE_BREAK
                | OpKind.WHILE_CONTINUE
                | OpKind.WHILE_END
            ):
                tc.check_while_block(op)
            case OpKind.FN_CALL | OpKind.FN_EXEC | OpKind.FN_PUSH | OpKind.FN_RETURN:
                op_index = tc.check_function_ops(op, ops, op_index, function)
            case (
                OpKind.PUSH_INT
                | OpKind.PUSH_STR
                | OpKind.PUSH_BOOL
                | OpKind.PUSH_CHAR
                | OpKind.PUSH_ARRAY
                | OpKind.FSTRING_CONCAT
            ):
                tc.check_literals(op, function)
            case OpKind.PRINT:
                tc.check_io(op)
            case OpKind.TYPEOF:
                tc.check_typeof(op)
            case (
                OpKind.SYSCALL0
                | OpKind.SYSCALL1
                | OpKind.SYSCALL2
                | OpKind.SYSCALL3
                | OpKind.SYSCALL4
                | OpKind.SYSCALL5
                | OpKind.SYSCALL6
            ):
                tc.check_syscalls(op)
            case OpKind.ARGC:
                tc.stack_push("int")
            case OpKind.ARGV:
                tc.stack_push("ptr")
            case OpKind.PUSH_ENUM_VARIANT:
                tc.check_enum_variant(op)
            case OpKind.IS_CHECK:
                tc.check_is(op, function)
            case OpKind.MATCH_START | OpKind.MATCH_ARM | OpKind.MATCH_END:
                tc.check_match(op, function)
            case OpKind.STRUCT_NEW:
                tc.check_struct_new(op)
            case OpKind.STRUCT_LITERAL:
                tc.check_struct_literal(op)
            case OpKind.METHOD_CALL:
                op_index = tc.check_method_call(op, ops, op_index, function)
            case OpKind.TYPE_CAST:
                tc.stack_pop()
                tc.stack_push(op.value)
            case OpKind.INCLUDE_FILE:
                pass
            case OpKind.IDENTIFIER:
                raise AssertionError("Identifiers should be resolved by the parser")
            case (
                OpKind.PRINT_BOOL
                | OpKind.PRINT_CHAR
                | OpKind.PRINT_CSTR
                | OpKind.PRINT_INT
                | OpKind.PRINT_STR
            ):
                assert False, "PRINT variants are resolved by the type checker"
            case _:
                assert_never(op.kind)

    fn_location = function.location if function else None

    if tc.return_types:
        unified = _stacks_compatible(tc.return_types, tc.stack)
        if unified is None:
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                "Invalid return types",
                fn_location,
                expected=str(tc.return_types),
                got=str(tc.stack),
            )
        tc.return_types = unified
        tc.stack = unified

    inferred_signature = Signature(tc.parameters, tc.stack)

    if function and function.signature and function.signature.type_vars:
        param_tvs: set[str] = set()
        for p in function.signature.parameters:
            if p.typ in function.signature.type_vars:
                param_tvs.add(p.typ)
            gen_params = extract_generic_params(p.typ)
            if gen_params:
                for gp in gen_params:
                    if gp in function.signature.type_vars:
                        param_tvs.add(gp)
            fn_sig_str = extract_fn_signature_str(p.typ)
            if fn_sig_str:
                for type_var in function.signature.type_vars:
                    if type_var in fn_sig_str:
                        param_tvs.add(type_var)
        ret_only = {
            r
            for r in function.signature.return_types
            if r in function.signature.type_vars and r not in param_tvs
        }
        if ret_only:
            names = ", ".join(sorted(ret_only))
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"Type variable(s) `{names}` appear in return types"
                f" but not in parameters of `{function.name}`",
                fn_location,
            )

    if (
        function
        and function.signature
        and not function.signature.type_vars
        and not function.signature.matches(inferred_signature)
    ):
        unused = _check_passthrough_params(function.signature, inferred_signature)
        if unused is None:
            raise_error(
                ErrorKind.SIGNATURE_MISMATCH,
                f"Invalid signature for function `{function.name}`",
                fn_location,
                expected=str(function.signature),
                got=str(inferred_signature),
                got_label="Inferred",
            )
        for param in unused:
            WARNINGS.append(
                CasaWarning(
                    WarningKind.UNUSED_PARAMETER,
                    f"Unused parameter `{param.typ}` in function `{function.name}`",
                    fn_location,
                )
            )
        inferred_signature = function.signature

    return inferred_signature  # type_check_ops


def type_check_functions(functions: Iterable[Function]):
    """Typecheck the given functions, collecting all errors."""
    all_errors: list[CasaError] = []
    for function in list(functions):
        if function.is_typechecked:
            continue
        # Skip lambda functions -- they are typechecked as part of their
        # parent function when it is resolved and typechecked.
        if function.name.startswith("lambda__"):
            continue
        function.is_typechecked = True
        if not function.is_used:
            function.ops = resolve_identifiers(function.ops, function)
        try:
            signature = type_check_ops(function.ops, function)
        except CasaErrorCollection as exc:
            all_errors.extend(exc.errors)
            continue
        if not function.signature:
            function.signature = signature
    if all_errors:
        raise CasaErrorCollection(all_errors)
