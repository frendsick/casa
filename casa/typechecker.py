from dataclasses import dataclass, field
from typing import assert_never
from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_VARIABLES,
    Function,
    Op,
    OpKind,
    Signature,
    Type,
    Variable,
)


ANY_TYPE = "any"


@dataclass
class TypeChecker:
    ops: list[Op]
    stack: list[Type] = field(default_factory=list)
    parameters: list[Type] = field(default_factory=list)

    def stack_push(self, typ: Type):
        self.stack.append(typ)

    def stack_peek(self) -> Type:
        if not self.stack:
            return ANY_TYPE
        return self.stack[-1]

    def stack_pop(self) -> Type:
        if not self.stack:
            self.parameters.append(ANY_TYPE)
            return ANY_TYPE
        return self.stack.pop()

    def expect_type(self, expected: Type) -> Type:
        if not self.stack:
            self.parameters.append(expected)
            return expected

        typ = self.stack.pop()
        if expected == ANY_TYPE:
            return typ
        if typ == ANY_TYPE:
            return expected
        if typ == expected:
            return typ
        raise TypeError(f"Expected `{expected}` but got `{typ}`")

    def apply_signature(self, signature: Signature):
        for expected in signature.parameters:
            self.expect_type(expected)
        for return_type in signature.return_types:
            self.stack_push(return_type)


def get_list_literal_type(op: Op) -> Type:
    list_items = op.value
    assert isinstance(list_items, list), "Expected list"

    if not list_items:
        return "list"

    item_type = get_literal_type(list_items[0])
    assert isinstance(item_type, str), "Expected non-generic type"

    return f"list[{item_type}]"


def get_literal_type(op: Op) -> Type | None:
    match op.kind:
        case OpKind.PUSH_INT:
            return "int"
        case OpKind.PUSH_LIST:
            return "list"
        case _:
            return None


def type_check_ops(ops: list[Op], function: Function | None = None) -> Signature:
    tc = TypeChecker(ops=ops)

    for op in ops:
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

                    if not global_variable.typ or global_variable.typ == ANY_TYPE and stack_type != ANY_TYPE:
                        global_variable.typ = stack_type

                    if global_variable.typ not in (stack_type, ANY_TYPE):
                        raise ValueError(
                            f"Cannot override global variable `{global_variable.name}` of type `{global_variable.typ}` with other type `{stack_type}`"
                        )

                    tc.expect_type(stack_type)
                    continue

                # Local variable
                assert isinstance(function, Function), "Expected function"
                for variable in function.variables:
                    if variable.name == variable_name:
                        if not variable.typ or variable.typ == ANY_TYPE and stack_type != ANY_TYPE:
                            variable.typ = stack_type

                        if variable.typ not in (stack_type, ANY_TYPE):
                            raise ValueError(
                                f"Cannot override local variable `{variable.name}` of type `{variable.typ}` with other type `{stack_type}`"
                            )

                        tc.expect_type(stack_type)
                        break
                else:
                    raise AssertionError(
                        f"Function `{function.name}` does not have variable `{variable_name}`"
                    )
            case OpKind.CALL_FN:
                function_name = op.value
                assert isinstance(function_name, str), "Expected function name"
                function_name = function_name
                global_function = GLOBAL_FUNCTIONS.get(function_name)
                assert global_function, "Expected function"

                if global_function.signature is None:
                    global_function.signature = type_check_ops(
                        global_function.ops, global_function
                    )
                tc.apply_signature(global_function.signature)
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
            case OpKind.EXEC_FN:
                fn_ptr = tc.stack_pop()
                assert isinstance(fn_ptr, str), "Function pointer type"
                start = fn_ptr.index("[") + 1
                end = fn_ptr.index("]", start)
                tc.apply_signature(Signature.from_str(fn_ptr[start:end]))
            case OpKind.GE:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.GT:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.IDENTIFIER:
                raise AssertionError("Identifiers should be resolved by the parser")
            case OpKind.IF_CONDITION:
                tc.expect_type("bool")
            case OpKind.IF_ELIF:
                pass
            case OpKind.IF_ELSE:
                pass
            case OpKind.IF_END:
                pass
            case OpKind.IF_START:
                pass
            case OpKind.LE:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
            case OpKind.LOAD:
                tc.expect_type("ptr")
                tc.stack_push(ANY_TYPE)
            case OpKind.LT:
                tc.stack_pop()
                tc.stack_pop()
                tc.stack_push("bool")
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
                tc.stack_pop()
            case OpKind.PUSH_FN:
                assert isinstance(op.value, str), "Expected identifier name"
                function_name = op.value
                assert isinstance(function_name, str), "Expected function name"
                global_function = GLOBAL_FUNCTIONS.get(function_name)
                assert isinstance(global_function, Function), "Expected function"

                if not global_function.signature:
                    global_function.signature = type_check_ops(
                        global_function.ops, global_function
                    )
                tc.stack_push(f"fn[{global_function.signature}]")
            case OpKind.PUSH_INT:
                tc.stack_push("int")
            case OpKind.PUSH_LIST:
                list_type = get_list_literal_type(op)
                tc.stack_push(list_type)
            case OpKind.PUSH_STR:
                tc.stack_push("str")
            case OpKind.PUSH_VARIABLE:
                variable_name = op.value
                assert isinstance(op.value, str), "Expected variable name"

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
                    raise NameError(
                        f"Function `{function.name}` does not have variable `{variable_name}`"
                    )
            case OpKind.ROT:
                t1 = tc.stack_pop()
                t2 = tc.stack_pop()
                t3 = tc.stack_pop()
                tc.stack_push(t2)
                tc.stack_push(t1)
                tc.stack_push(t3)
            case OpKind.STORE:
                tc.expect_type("ptr")
                tc.stack_pop()
            case OpKind.SUB:
                tc.expect_type("int")
                t1 = tc.stack_pop()
                tc.stack_push(t1)
            case OpKind.SWAP:
                t1 = tc.stack_pop()
                t2 = tc.stack_pop()
                tc.stack_push(t1)
                tc.stack_push(t2)
            case OpKind.WHILE_CONDITION:
                tc.expect_type("bool")
            case OpKind.WHILE_END:
                pass
            case OpKind.WHILE_START:
                pass
            case _:
                assert_never(op.kind)

    return Signature(tc.parameters, tc.stack)  # type_check_ops
