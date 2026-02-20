from dataclasses import dataclass, field
from collections.abc import Iterable
from typing import assert_never

from casa.common import (
    ANY_TYPE,
    GLOBAL_FUNCTIONS,
    GLOBAL_VARIABLES,
    Function,
    Location,
    Op,
    OpKind,
    Parameter,
    Signature,
    Struct,
    Type,
    Variable,
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
    before: list[Type]
    after: list[Type]
    before_origins: list[Location | None]
    after_origins: list[Location | None]
    condition_present: bool
    default_present: bool

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


@dataclass
class TypeChecker:
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
        self.stack.append(typ)
        self.stack_origins.append(origin or self.current_location)

    def stack_peek(self) -> Type:
        if not self.stack:
            return ANY_TYPE
        return self.stack[-1]

    def stack_pop(self) -> Type:
        if not self.stack:
            self.parameters.append(Parameter(ANY_TYPE))
            self.last_pop_origin = None
            return ANY_TYPE
        self.last_pop_origin = self.stack_origins.pop()
        return self.stack.pop()

    def expect_type(self, expected: Type) -> Type:
        if not self.stack:
            self.parameters.append(Parameter(expected))
            self.last_pop_origin = None
            return expected

        origin = self.stack_origins.pop()
        typ = self.stack.pop()
        self.last_pop_origin = origin
        if expected == ANY_TYPE:
            return typ
        if typ == ANY_TYPE:
            return expected
        if typ == expected:
            return typ
        if expected == "fn" and typ.startswith("fn"):
            start = typ.index("[") + 1
            end = typ.index("]", start)
            signature = Signature.from_str(typ[start:end])
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
        note = None
        if origin and origin != self.current_location:
            note = (f"value of type `{typ}` pushed here", origin)
        raise_error(
            ErrorKind.TYPE_MISMATCH,
            message,
            self.current_location,
            expected=expected_str,
            got=f"`{typ}`",
            note=note,
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
            if expected.typ not in signature.type_vars:
                self.expect_type(expected.typ)
                continue
            self._bind_type_var(bindings, expected.typ, self.stack_pop())
        self.current_expect_context = None

        for return_type in signature.return_types:
            resolved = bindings.get(return_type, return_type)
            self.stack_push(resolved)


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

    n_unused = n_declared - n_inferred

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
    OpKind.ASSIGN_DECREMENT: ("-=", "int -> None"),
    OpKind.ASSIGN_INCREMENT: ("+=", "int -> None"),
    OpKind.HEAP_ALLOC: ("alloc", "int -> ptr"),
    OpKind.IF_CONDITION: ("if condition", "bool -> None"),
    OpKind.WHILE_CONDITION: ("while condition", "bool -> None"),
}


def type_check_ops(ops: list[Op], function: Function | None = None) -> Signature:
    assert len(OpKind) == 55, "Exhaustive handling for `OpKind`"

    tc = TypeChecker(ops=ops)
    for op in ops:
        tc.current_location = op.location
        tc.current_expect_context = None
        effect = OP_STACK_EFFECTS.get(op.kind)
        if effect:
            name, sig = effect
            tc.current_op_context = f"`{name}` ({sig})"
        else:
            tc.current_op_context = None
        match op.kind:
            case OpKind.ADD:
                tc.expect_type("int")
                t1 = tc.stack_pop()
                tc.stack_push(t1)
            case OpKind.AND:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.DIV:
                tc.expect_type("int")
                tc.expect_type("int")
                tc.stack_push("int")
            case OpKind.ASSIGN_DECREMENT:
                tc.expect_type("int")
            case OpKind.ASSIGN_INCREMENT:
                tc.expect_type("int")
            case OpKind.ASSIGN_VARIABLE:
                variable_name = op.value
                assert isinstance(variable_name, str), "Expected variable name"

                stack_type = tc.stack_peek()

                # Global variable
                global_variable = GLOBAL_VARIABLES.get(variable_name)
                if global_variable:
                    assert isinstance(
                        global_variable, Variable
                    ), "Valid global variable"

                    if not global_variable.typ or (
                        global_variable.typ == ANY_TYPE and stack_type != ANY_TYPE
                    ):
                        global_variable.typ = stack_type

                    if global_variable.typ not in (stack_type, ANY_TYPE):
                        raise_error(
                            ErrorKind.INVALID_VARIABLE,
                            f"Cannot override global variable `{global_variable.name}` of type `{global_variable.typ}` with other type `{stack_type}`",
                            op.location,
                        )

                    tc.expect_type(stack_type)
                    continue

                # Local variable
                assert isinstance(function, Function), "Expected function"
                for variable in function.variables:
                    if variable.name == variable_name:
                        if not variable.typ or (
                            variable.typ == ANY_TYPE and stack_type != ANY_TYPE
                        ):
                            variable.typ = stack_type

                        if (
                            variable.typ not in (stack_type, ANY_TYPE)
                            and stack_type != ANY_TYPE
                        ):
                            raise_error(
                                ErrorKind.INVALID_VARIABLE,
                                f"Cannot override local variable `{variable.name}` of type `{variable.typ}` with other type `{stack_type}`",
                                op.location,
                            )

                        tc.expect_type(stack_type)
                        break
                else:
                    raise AssertionError(
                        f"Function `{function.name}` does not have variable `{variable_name}`"
                    )
            case OpKind.DROP:
                tc.stack_pop()
            case OpKind.DUP:
                t1 = tc.stack_pop()
                origin = tc.last_pop_origin
                tc.stack_push(t1, origin)
                tc.stack_push(t1, origin)
            case OpKind.EQ:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.FN_CALL:
                function_name = op.value
                assert isinstance(function_name, str), "Expected function name"

                function_name = function_name
                global_function = GLOBAL_FUNCTIONS.get(function_name)
                assert global_function, "Expected function"

                if not global_function.is_typechecked:
                    global_function.is_typechecked = True
                    signature = type_check_ops(global_function.ops, global_function)
                    if not global_function.signature:
                        global_function.signature = signature

                assert global_function.signature, "Signature is defined"
                tc.apply_signature(global_function.signature, function_name)
            case OpKind.FN_EXEC:
                # Lambdas from other functions are typed as `any`
                fn_symmetrical = "fn"
                fn_ptr = tc.stack_peek()
                if fn_ptr == ANY_TYPE:
                    fn_ptr = "fn"

                fn_ptr = tc.expect_type(fn_ptr)
                assert isinstance(fn_ptr, str), "Function pointer type"

                if fn_ptr == fn_symmetrical:
                    continue

                start = fn_ptr.index("[") + 1
                end = fn_ptr.index("]", start)
                tc.apply_signature(Signature.from_str(fn_ptr[start:end]), "exec")
            case OpKind.FN_RETURN:
                if tc.return_types is None:
                    tc.return_types = tc.stack.copy()

                if tc.return_types != tc.stack:
                    raise_error(
                        ErrorKind.TYPE_MISMATCH,
                        "Invalid return types",
                        op.location,
                        expected=str(tc.return_types),
                        got=str(tc.stack),
                    )

                if tc.branched_stacks:
                    branched = tc.branched_stacks[-1]
                    tc.stack = branched.before.copy()
                    tc.stack_origins = branched.before_origins.copy()
            case OpKind.FN_PUSH:
                assert isinstance(op.value, str), "Expected identifier name"
                function_name = op.value
                assert isinstance(function_name, str), "Expected function name"
                global_function = GLOBAL_FUNCTIONS.get(function_name)
                assert isinstance(global_function, Function), "Expected function"

                if not global_function.is_typechecked:
                    global_function.signature = type_check_ops(
                        global_function.ops, global_function
                    )
                    global_function.is_typechecked = True
                tc.stack_push(f"fn[{global_function.signature}]")
            case OpKind.GE:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.GT:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.HEAP_ALLOC:
                tc.expect_type("int")
                tc.stack_push("ptr")
            case OpKind.IDENTIFIER:
                raise AssertionError("Identifiers should be resolved by the parser")
            case OpKind.IF_CONDITION:
                tc.expect_type("bool")

                assert len(tc.branched_stacks) > 0, "If block stack state is saved"
                branched = tc.branched_stacks[-1]

                # First condition is run anyways
                if not branched.condition_present:
                    branched.condition_present = True
                    branched.before = tc.stack.copy()
                    branched.before_origins = tc.stack_origins.copy()
                    branched.after = branched.before
                    branched.after_origins = branched.before_origins
                    continue

                if tc.stack != branched.before:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed across branch",
                        op.location,
                        expected=str(branched.before),
                        got=str(tc.stack),
                    )
            case OpKind.IF_ELIF | OpKind.IF_ELSE:
                assert tc.branched_stacks, "If block stack state is saved"
                branched = tc.branched_stacks[-1]
                before, after = branched.before, branched.after

                if op.kind is OpKind.IF_ELSE:
                    branched.default_present = True

                if tc.stack == before == after:
                    continue
                if tc.stack == after and before != after:
                    tc.stack = before.copy()
                    tc.stack_origins = branched.before_origins.copy()
                    continue
                if before == after:
                    branched.after = tc.stack.copy()
                    branched.after_origins = tc.stack_origins.copy()
                    tc.stack = before.copy()
                    tc.stack_origins = branched.before_origins.copy()
                    continue
                raise_error(
                    ErrorKind.STACK_MISMATCH,
                    "Stack state changed across branch",
                    op.location,
                    expected=str(branched.before),
                    got=str(tc.stack),
                )
            case OpKind.IF_END:
                branched = tc.branched_stacks.pop()
                if tc.stack == branched.before == branched.after:
                    continue
                if tc.stack == branched.after and branched.default_present:
                    tc.stack_origins = branched.after_origins.copy()
                    continue
                if tc.stack == branched.before and not branched.default_present:
                    continue
                raise_error(
                    ErrorKind.STACK_MISMATCH,
                    "Stack state changed across branch",
                    op.location,
                    expected=str(branched.before),
                    got=str(tc.stack),
                )
            case OpKind.IF_START:
                tc.branched_stacks.append(BranchedStack(tc.stack, tc.stack_origins))
            case OpKind.INCLUDE_FILE:
                pass
            case OpKind.LE:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.LOAD:
                # TODO: Expect pointer types
                tc.stack_pop()
                tc.stack_push(ANY_TYPE)
            case OpKind.LT:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.METHOD_CALL:
                method_name = op.value
                assert isinstance(method_name, str), "Expected method name"

                receiver = tc.stack_peek()
                function_name = f"{receiver}::{method_name}"

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

                    signature = type_check_ops(global_function.ops, global_function)
                    if not global_function.signature:
                        global_function.signature = signature
                    global_function.is_typechecked = True

                assert global_function.signature, "Signature is defined"
                tc.apply_signature(global_function.signature, function_name)

                op.value = function_name
                op.kind = OpKind.FN_CALL
            case OpKind.MOD:
                tc.expect_type("int")
                tc.expect_type("int")
                tc.stack_push("int")
            case OpKind.MUL:
                tc.expect_type("int")
                tc.expect_type("int")
                tc.stack_push("int")
            case OpKind.NE:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.NOT:
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.OR:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.OVER:
                t1 = tc.stack_pop()
                o1 = tc.last_pop_origin
                t2 = tc.stack_pop()
                o2 = tc.last_pop_origin
                tc.stack_push(t2, o2)
                tc.stack_push(t1, o1)
                tc.stack_push(t2, o2)
            case OpKind.PRINT:
                typ = tc.stack_pop()
                if typ == "str":
                    op.kind = OpKind.PRINT_STR
                else:
                    op.kind = OpKind.PRINT_INT
            case OpKind.PRINT_INT | OpKind.PRINT_STR:
                assert False, "PRINT_INT and PRINT_STR are resolved by the type checker"
            case OpKind.PUSH_ARRAY:
                # TODO: Fine-grain array typing
                tc.stack_push("array")
            case OpKind.PUSH_BOOL:
                tc.stack_push("bool")
            case OpKind.PUSH_CAPTURE:
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
                    tc.stack_push(capture.typ)
                    continue

                if global_variable := GLOBAL_VARIABLES.get(capture.name):
                    assert global_variable.typ, "Variable type"
                    capture.typ = global_variable.typ
                    tc.stack_push(global_variable.typ)
                    continue

                raise AssertionError(
                    f"Capture `{capture.name}` has not been type checked before its usage"
                )
            case OpKind.PUSH_INT:
                tc.stack_push("int")
            case OpKind.PUSH_STR:
                tc.stack_push("str")
            case OpKind.PUSH_VARIABLE:
                variable_name = op.value
                assert isinstance(variable_name, str), "Expected variable name"

                # Global variable
                global_variable = GLOBAL_VARIABLES.get(variable_name)
                if global_variable:
                    assert global_variable.typ, "Global variable type should be defined"
                    tc.stack_push(global_variable.typ)
                    continue

                # Local variable
                assert isinstance(function, Function), "Expected function"
                for variable in function.variables:
                    if variable == variable_name:
                        if not variable.typ:
                            raise AssertionError(
                                f"Variable `{variable.name}` has not been type checked before its usage"
                            )
                        tc.stack_push(variable.typ)
                        break
                else:
                    raise_error(
                        ErrorKind.UNDEFINED_NAME,
                        f"Function `{function.name}` does not have variable `{variable_name}`",
                        op.location,
                    )
            case OpKind.ROT:
                t1 = tc.stack_pop()
                o1 = tc.last_pop_origin
                t2 = tc.stack_pop()
                o2 = tc.last_pop_origin
                t3 = tc.stack_pop()
                o3 = tc.last_pop_origin
                tc.stack_push(t2, o2)
                tc.stack_push(t1, o1)
                tc.stack_push(t3, o3)
            case OpKind.SHL | OpKind.SHR:
                tc.expect_type("int")
                t1 = tc.stack_pop()
                tc.stack_push(t1)
            case OpKind.STORE:
                # TODO: Expect pointer types
                tc.stack_pop()
                tc.stack_pop()
            case OpKind.STRUCT_NEW:
                struct = op.value
                assert isinstance(struct, Struct), "Expected struct"

                member_types = " ".join(m.typ for m in struct.members)
                tc.current_op_context = (
                    f"`{struct.name}` ({member_types} -> {struct.name})"
                )
                for member in struct.members:
                    tc.current_expect_context = (
                        f"member `{member.name}` of `{struct.name}`"
                    )
                    tc.expect_type(member.typ)
                tc.current_expect_context = None
                tc.stack_push(struct.name)
            case OpKind.SUB:
                tc.expect_type("int")
                t1 = tc.stack_pop()
                tc.stack_push(t1)
            case OpKind.SWAP:
                t1 = tc.stack_pop()
                o1 = tc.last_pop_origin
                t2 = tc.stack_pop()
                o2 = tc.last_pop_origin
                tc.stack_push(t1, o1)
                tc.stack_push(t2, o2)
            case OpKind.TYPE_CAST:
                cast_type = op.value
                assert isinstance(cast_type, str), "Expected cast type"
                tc.stack_pop()
                tc.stack_push(cast_type)
            case OpKind.WHILE_BREAK:
                assert len(tc.branched_stacks) > 0, "While block stack state is saved"
                branched = tc.branched_stacks[-1]

                if tc.stack != branched.after:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed in loop",
                        op.location,
                        expected=str(branched.after),
                        got=str(tc.stack),
                    )
            case OpKind.WHILE_CONDITION:
                tc.expect_type("bool")

                assert len(tc.branched_stacks) > 0, "While block stack state is saved"
                branched = tc.branched_stacks[-1]
                branched.condition_present = True

                if tc.stack != branched.before:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed in loop",
                        op.location,
                        expected=str(branched.before),
                        got=str(tc.stack),
                    )
            case OpKind.WHILE_END:
                branched = tc.branched_stacks.pop()
                if branched.after != tc.stack:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed in loop",
                        op.location,
                        expected=str(branched.after),
                        got=str(tc.stack),
                    )
            case OpKind.WHILE_CONTINUE:
                assert len(tc.branched_stacks) > 0, "While block stack state is saved"
                branched = tc.branched_stacks[-1]

                if tc.stack != branched.after:
                    raise_error(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed in loop",
                        op.location,
                        expected=str(branched.after),
                        got=str(tc.stack),
                    )
            case OpKind.WHILE_START:
                tc.branched_stacks.append(BranchedStack(tc.stack, tc.stack_origins))
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
        param_tvs = {
            p.typ
            for p in function.signature.parameters
            if p.typ in function.signature.type_vars
        }
        ret_only = {
            r
            for r in function.signature.return_types
            if r in function.signature.type_vars and r not in param_tvs
        }
        if ret_only:
            names = ", ".join(sorted(ret_only))
            raise_error(
                ErrorKind.TYPE_MISMATCH,
                f"Type variable(s) `{names}` appear in return types but not in parameters of `{function.name}`",
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
    for fn in list(functions):
        if fn.is_typechecked:
            continue
        fn.is_typechecked = True
        if not fn.is_used:
            fn.ops = resolve_identifiers(fn.ops, fn)
        try:
            signature = type_check_ops(fn.ops, fn)
        except CasaErrorCollection as exc:
            all_errors.extend(exc.errors)
            continue
        if not fn.signature:
            fn.signature = signature
    if all_errors:
        raise CasaErrorCollection(all_errors)
