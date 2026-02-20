from dataclasses import dataclass, field
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
from casa.error import CasaError, CasaErrorCollection, ErrorKind
from casa.parser import resolve_identifiers


@dataclass
class BranchedStack:
    before: list[Type]
    after: list[Type]
    condition_present: bool
    default_present: bool

    def __init__(self, before: list[Type], after: list[Type] | None = None):
        self.before = before.copy()
        self.after = after.copy() if after else before.copy()
        self.default_present = False
        self.condition_present = False


@dataclass
class TypeChecker:
    ops: list[Op]
    stack: list[Type] = field(default_factory=list)
    parameters: list[Parameter] = field(default_factory=list)
    # Saved on `return`
    return_types: list[Type] | None = None
    # Store stack states before each conditional and loop block
    branched_stacks: list[BranchedStack] = field(default_factory=list)
    # Current op location for error reporting
    current_location: Location | None = None

    def stack_push(self, typ: Type):
        self.stack.append(typ)

    def stack_peek(self) -> Type:
        if not self.stack:
            return ANY_TYPE
        return self.stack[-1]

    def stack_pop(self) -> Type:
        if not self.stack:
            self.parameters.append(Parameter(ANY_TYPE))
            return ANY_TYPE
        return self.stack.pop()

    def expect_type(self, expected: Type) -> Type:
        if not self.stack:
            self.parameters.append(Parameter(expected))
            return expected

        typ = self.stack.pop()
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
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.TYPE_MISMATCH,
                        "Expected symmetrical function type",
                        self.current_location,
                    )
                )
            return typ
        raise CasaErrorCollection(
            CasaError(
                ErrorKind.TYPE_MISMATCH,
                "Type mismatch",
                self.current_location,
                expected=f"`{expected}`",
                got=("Got", f"`{typ}`"),
            )
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
            raise CasaErrorCollection(
                CasaError(
                    ErrorKind.TYPE_MISMATCH,
                    f"Type variable `{type_var}` bound to `{bound}` but got `{actual}`",
                    self.current_location,
                )
            )

    def apply_signature(self, signature: Signature):
        if not signature.type_vars:
            for expected in signature.parameters:
                self.expect_type(expected.typ)
            for return_type in signature.return_types:
                self.stack_push(return_type)
            return

        # Bind type variables to actual stack types
        bindings: dict[str, Type] = {}
        for expected in signature.parameters:
            if expected.typ not in signature.type_vars:
                self.expect_type(expected.typ)
                continue
            self._bind_type_var(bindings, expected.typ, self.stack_pop())

        for return_type in signature.return_types:
            resolved = bindings.get(return_type, return_type)
            self.stack_push(resolved)


def type_check_ops(ops: list[Op], function: Function | None = None) -> Signature:
    assert len(OpKind) == 55, "Exhaustive handling for `OpKind`"

    tc = TypeChecker(ops=ops)
    for op in ops:
        tc.current_location = op.location
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
                        raise CasaErrorCollection(
                            CasaError(
                                ErrorKind.INVALID_VARIABLE,
                                f"Cannot override global variable `{global_variable.name}` of type `{global_variable.typ}` with other type `{stack_type}`",
                                op.location,
                            )
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
                            raise CasaErrorCollection(
                                CasaError(
                                    ErrorKind.INVALID_VARIABLE,
                                    f"Cannot override local variable `{variable.name}` of type `{variable.typ}` with other type `{stack_type}`",
                                    op.location,
                                )
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
                tc.stack_push(t1)
                tc.stack_push(t1)
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
                tc.apply_signature(global_function.signature)
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
                tc.apply_signature(Signature.from_str(fn_ptr[start:end]))
            case OpKind.FN_RETURN:
                if tc.return_types is None:
                    tc.return_types = tc.stack.copy()

                if tc.return_types != tc.stack:
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.TYPE_MISMATCH,
                            "Invalid return types",
                            op.location,
                            expected=str(tc.return_types),
                            got=("Got", str(tc.stack)),
                        )
                    )

                if tc.branched_stacks:
                    branched = tc.branched_stacks[-1]
                    tc.stack = branched.before.copy()
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
                    branched.after = branched.before
                    continue

                if tc.stack != branched.before:
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.STACK_MISMATCH,
                            "Stack state changed across branch",
                            op.location,
                            expected=str(branched.before),
                            got=("Got", str(tc.stack)),
                        )
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
                    continue
                if before == after:
                    branched.after = tc.stack.copy()
                    tc.stack = before.copy()
                    continue
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed across branch",
                        op.location,
                        expected=str(branched.before),
                        got=("Got", str(tc.stack)),
                    )
                )
            case OpKind.IF_END:
                branched = tc.branched_stacks.pop()
                if (
                    tc.stack == branched.before == branched.after
                    or (tc.stack == branched.after and branched.default_present)
                    or (tc.stack == branched.before and not branched.default_present)
                ):
                    continue
                raise CasaErrorCollection(
                    CasaError(
                        ErrorKind.STACK_MISMATCH,
                        "Stack state changed across branch",
                        op.location,
                        expected=str(branched.before),
                        got=("Got", str(tc.stack)),
                    )
                )
            case OpKind.IF_START:
                tc.branched_stacks.append(BranchedStack(tc.stack))
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
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.UNDEFINED_NAME,
                            f"Method `{function_name}` does not exist",
                            op.location,
                        )
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
                tc.apply_signature(global_function.signature)

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
                t2 = tc.stack_pop()
                tc.stack_push(t2)
                tc.stack_push(t1)
                tc.stack_push(t2)
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
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.UNDEFINED_NAME,
                            f"Function `{function.name}` does not have capture `{capture_name}`",
                            op.location,
                        )
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
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.UNDEFINED_NAME,
                            f"Function `{function.name}` does not have variable `{variable_name}`",
                            op.location,
                        )
                    )
            case OpKind.ROT:
                t1 = tc.stack_pop()
                t2 = tc.stack_pop()
                t3 = tc.stack_pop()
                tc.stack_push(t2)
                tc.stack_push(t1)
                tc.stack_push(t3)
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

                for member in struct.members:
                    tc.expect_type(member.typ)
                tc.stack_push(struct.name)
            case OpKind.SUB:
                tc.expect_type("int")
                t1 = tc.stack_pop()
                tc.stack_push(t1)
            case OpKind.SWAP:
                t1 = tc.stack_pop()
                t2 = tc.stack_pop()
                tc.stack_push(t1)
                tc.stack_push(t2)
            case OpKind.TYPE_CAST:
                cast_type = op.value
                assert isinstance(cast_type, str), "Expected cast type"
                tc.stack_pop()
                tc.stack_push(cast_type)
            case OpKind.WHILE_BREAK:
                assert len(tc.branched_stacks) > 0, "While block stack state is saved"
                branched = tc.branched_stacks[-1]

                if tc.stack != branched.after:
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.STACK_MISMATCH,
                            "Stack state changed in loop",
                            op.location,
                            expected=str(branched.after),
                            got=("Got", str(tc.stack)),
                        )
                    )
            case OpKind.WHILE_CONDITION:
                tc.expect_type("bool")

                assert len(tc.branched_stacks) > 0, "While block stack state is saved"
                branched = tc.branched_stacks[-1]
                branched.condition_present = True

                if tc.stack != branched.before:
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.STACK_MISMATCH,
                            "Stack state changed in loop",
                            op.location,
                            expected=str(branched.before),
                            got=("Got", str(tc.stack)),
                        )
                    )
            case OpKind.WHILE_END:
                branched = tc.branched_stacks.pop()
                if branched.after != tc.stack:
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.STACK_MISMATCH,
                            "Stack state changed in loop",
                            op.location,
                            expected=str(branched.after),
                            got=("Got", str(tc.stack)),
                        )
                    )
            case OpKind.WHILE_CONTINUE:
                assert len(tc.branched_stacks) > 0, "While block stack state is saved"
                branched = tc.branched_stacks[-1]

                if tc.stack != branched.after:
                    raise CasaErrorCollection(
                        CasaError(
                            ErrorKind.STACK_MISMATCH,
                            "Stack state changed in loop",
                            op.location,
                            expected=str(branched.after),
                            got=("Got", str(tc.stack)),
                        )
                    )
            case OpKind.WHILE_START:
                tc.branched_stacks.append(BranchedStack(tc.stack))
            case _:
                assert_never(op.kind)

    fn_location = function.location if function else None

    if tc.return_types and tc.return_types != tc.stack:
        raise CasaErrorCollection(
            CasaError(
                ErrorKind.TYPE_MISMATCH,
                "Invalid return types",
                fn_location,
                expected=str(tc.return_types),
                got=("Got", str(tc.stack)),
            )
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
            raise CasaErrorCollection(
                CasaError(
                    ErrorKind.TYPE_MISMATCH,
                    f"Type variable(s) `{names}` appear in return types but not in parameters of `{function.name}`",
                    fn_location,
                )
            )

    if (
        function
        and function.signature
        and not function.signature.type_vars
        and not function.signature.matches(inferred_signature)
    ):
        raise CasaErrorCollection(
            CasaError(
                ErrorKind.SIGNATURE_MISMATCH,
                f"Invalid signature for function `{function.name}`",
                fn_location,
                expected=str(function.signature),
                got=("Inferred", str(inferred_signature)),
            )
        )

    return inferred_signature  # type_check_ops
