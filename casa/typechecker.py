"""Stack-based type inference and checking for the Casa compiler."""

from dataclasses import dataclass, field
from collections.abc import Iterable
from typing import assert_never

from casa.common import (
    ANY_TYPE,
    GLOBAL_FUNCTIONS,
    GLOBAL_TRAITS,
    GLOBAL_VARIABLES,
    Function,
    Intrinsic,
    Location,
    Op,
    OpKind,
    Parameter,
    Signature,
    Struct,
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
        # Match parameterized generics with any wildcard, e.g. option[any] with option[int]
        exp_base = extract_generic_base(expected)
        act_base = extract_generic_base(typ)
        if exp_base is not None and exp_base == act_base:
            exp_params = extract_generic_params(expected)
            act_params = extract_generic_params(typ)
            if exp_params and act_params and len(exp_params) == len(act_params):
                if all(
                    expected_param == ANY_TYPE
                    or actual_param == ANY_TYPE
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
        if bound == ANY_TYPE and actual != ANY_TYPE:
            bindings[type_var] = actual
        elif actual != bound and actual != ANY_TYPE and bound != ANY_TYPE:
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

    def check_comparison(self) -> None:
        """Handle EQ, NE, LT, LE, GT, GE."""
        self.stack_pop()
        self.stack_pop()
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

            if (
                local_variable.typ not in (effective_type, ANY_TYPE)
                and effective_type != ANY_TYPE
            ):
                raise_error(
                    ErrorKind.INVALID_VARIABLE,
                    f"Cannot override local variable `{local_variable.name}`"
                    f" of type `{local_variable.typ}`"
                    f" with other type `{effective_type}`",
                    op.location,
                )

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

            if global_variable.typ not in (effective_type, ANY_TYPE):
                raise_error(
                    ErrorKind.INVALID_VARIABLE,
                    f"Cannot override global variable `{global_variable.name}`"
                    f" of type `{global_variable.typ}`"
                    f" with other type `{effective_type}`",
                    op.location,
                )

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

    def check_if_block(self, op: Op) -> bool:
        """Handle IF_START through IF_END. Returns True if caller should continue."""
        match op.kind:
            case OpKind.IF_START:
                bs = BranchedStack(self.stack, self.stack_origins)
                bs.if_location = op.location
                self.branched_stacks.append(bs)
            case OpKind.IF_CONDITION:
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
                    return True

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
                else:
                    branched.current_branch_label = "elif"
                branched.current_branch_location = op.location

                if self.stack == before == after:
                    return True
                unified_cur_after = _stacks_compatible(self.stack, after)
                if unified_cur_after is not None and before != after:
                    branched.after = unified_cur_after
                    self.stack = before.copy()
                    self.stack_origins = branched.before_origins.copy()
                    return True
                if before == after:
                    branched.after = self.stack.copy()
                    branched.after_origins = self.stack_origins.copy()
                    self.stack = before.copy()
                    self.stack_origins = branched.before_origins.copy()
                    return True
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
                    return True
                unified_cur_after = _stacks_compatible(self.stack, branched.after)
                if unified_cur_after is not None and branched.default_present:
                    self.stack = unified_cur_after
                    self.stack_origins = branched.after_origins.copy()
                    return True
                unified_cur_before = _stacks_compatible(self.stack, branched.before)
                if unified_cur_before is not None and not branched.default_present:
                    self.stack = unified_cur_before
                    self.stack_origins = branched.before_origins.copy()
                    return True
                raise_error(
                    ErrorKind.STACK_MISMATCH,
                    "Branches have incompatible stack effects",
                    op.location,
                    notes=_branch_mismatch_notes(branched),
                )
        return False

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
            case OpKind.FN_RETURN:
                if self.return_types is None:
                    self.return_types = self.stack.copy()
                if self.return_types != self.stack:
                    raise_error(
                        ErrorKind.TYPE_MISMATCH,
                        "Invalid return types",
                        op.location,
                        expected=str(self.return_types),
                        got=str(self.stack),
                    )
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
            case OpKind.PUSH_NONE:
                self.stack_push("option")
            case OpKind.SOME:
                t1 = self.stack_pop()
                self.stack_push(f"option[{t1}]")
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
        else:
            op.kind = OpKind.PRINT_INT

    def check_syscalls(self, op: Op) -> None:
        """Handle SYSCALL0 through SYSCALL6."""
        arg_count = int(op.kind.name[-1])
        self.expect_type("int")
        for _ in range(arg_count):
            self.stack_pop()
        self.stack_push("int")

    def check_struct_new(self, op: Op) -> None:
        """Handle STRUCT_NEW."""
        struct = op.value
        assert isinstance(struct, Struct), "Expected struct"
        member_types = " ".join(m.typ for m in struct.members)
        self.current_op_context = f"`{struct.name}` ({member_types} -> {struct.name})"
        for member in struct.members:
            self.current_expect_context = f"member `{member.name}` of `{struct.name}`"
            self.expect_type(member.typ)
        self.current_expect_context = None
        self.stack_push(struct.name)

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
}


def _infer_literal_type(op: Op, function: Function | None = None) -> str:
    """Infer the type of an array item Op."""
    match op.kind:
        case OpKind.PUSH_CHAR:
            return "char"
        case OpKind.PUSH_INT:
            return "int"
        case OpKind.PUSH_NONE:
            return "option"
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


def _unify_type(a: Type, b: Type) -> Type | None:
    """Resolve two compatible types to the more specific one.

    Returns the unified type, or None if the types are incompatible.
    """
    if a == b:
        return a
    if a == ANY_TYPE:
        return b
    if b == ANY_TYPE:
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
    assert len(OpKind) == 79, "Exhaustive handling for `OpKind`"

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
                tc.check_comparison()
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
                | OpKind.PUSH_NONE
                | OpKind.SOME
                | OpKind.PUSH_ARRAY
                | OpKind.FSTRING_CONCAT
            ):
                tc.check_literals(op, function)
            case OpKind.PRINT:
                tc.check_io(op)
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
            case OpKind.STRUCT_NEW:
                tc.check_struct_new(op)
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

    if tc.return_types and tc.return_types != tc.stack:
        raise_error(
            ErrorKind.TYPE_MISMATCH,
            "Invalid return types",
            fn_location,
            expected=str(tc.return_types),
            got=str(tc.stack),
        )

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
