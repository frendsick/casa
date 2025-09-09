from typing import assert_never

from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_VARIABLES,
    Function,
    GenericType,
    Op,
    OpKind,
    Signature,
    Type,
    Variable,
)


def apply_signature_check(
    signature: Signature,
    stack: list[Type],
    generics: dict[GenericType, Type],
):
    for expected in signature.parameters:
        typ = stack_peek(stack)
        if typ is None:
            raise IndexError("Stack underflow")

        if isinstance(expected, GenericType):
            generics.setdefault(expected, typ)
            expect_type(stack, typ)
        else:
            expect_type(stack, expected)

    for typ in signature.return_types:
        stack_push(stack, generics[typ] if isinstance(typ, GenericType) else typ)


def apply_signature_infer(
    signature: Signature,
    stack: list[Type],
    generics: dict[GenericType, Type],
    parameters: list[Type],
):
    for expected in signature.parameters:
        if stack:
            typ = stack_pop(stack)
            if isinstance(expected, GenericType):
                generics.setdefault(expected, typ)
            else:
                if isinstance(typ, GenericType):
                    generics.setdefault(typ, expected)
        else:
            parameters.insert(0, expected)

    for typ in signature.return_types:
        stack_push(stack, typ)


def get_signature_from_op(
    op: Op,
    stack: list[Type],
    function: Function | None = None,
) -> Signature:
    assert len(OpKind) == 41, "Exhaustive handling for `OpKind`"

    match op.kind:
        case OpKind.AND | OpKind.OR:
            return Signature(parameters=["any", "any"], return_types=["bool"])
        case OpKind.ADD | OpKind.DIV | OpKind.MOD | OpKind.MUL | OpKind.SUB:
            return Signature(parameters=["int", "int"], return_types=["int"])
        case OpKind.ASSIGN_DECREMENT:
            return Signature(parameters=["int"], return_types=[])
        case OpKind.ASSIGN_INCREMENT:
            return Signature(parameters=["int"], return_types=[])
        case OpKind.ASSIGN_VARIABLE:
            variable_name = op.value
            assert isinstance(variable_name, str), "Expected variable name"

            stack_type = stack_peek(stack)
            if not stack_type:
                raise IndexError("Stack underflow")

            # Global variable
            global_variable = GLOBAL_VARIABLES.get(variable_name)
            if global_variable:
                assert isinstance(global_variable, Variable), "Valid global variable"
                if global_variable.typ and global_variable.typ != stack_type:
                    raise ValueError(
                        f"Cannot override global variable of type `{global_variable.typ}` with other type `{stack_type}`"
                    )
                global_variable.typ = stack_type
                return Signature(parameters=[stack_type], return_types=[])

            # Local variable
            assert isinstance(function, Function), "Expected function"
            for variable in function.variables:
                if variable.name == variable_name:
                    if variable.typ and variable.typ != stack_type:
                        raise ValueError(
                            f"Cannot override local variable of type `{variable.typ}` with other type `{stack_type}`"
                        )
                    variable.typ = stack_type
                    return Signature(parameters=[stack_type], return_types=[])

            raise AssertionError(
                f"Function `{function.name}` does not have variable `{variable_name}`"
            )
        case OpKind.CALL_FN:
            assert isinstance(op.value, str), "Expected identifier name"
            function_name = op.value
            function = GLOBAL_FUNCTIONS.get(function_name)

            assert isinstance(function, Function), "Expected function"
            assert isinstance(
                function.signature, Signature
            ), "Expected function signature"

            if not function.is_typechecked:
                function.is_typechecked = True
                type_check_ops(function.ops, function)
            return function.signature
        case OpKind.DROP:
            return Signature(parameters=["any"], return_types=[])
        case OpKind.DUP:
            t1 = GenericType("T1")
            return Signature(parameters=[t1], return_types=[t1, t1])
        case OpKind.EQ | OpKind.GE | OpKind.GT | OpKind.LE | OpKind.LT | OpKind.NE:
            t1 = GenericType("T1")
            return Signature(parameters=[t1, t1], return_types=["bool"])
        case OpKind.EXEC_FN:
            if not stack:
                raise IndexError("Stack underflow")
            fn_ptr = stack_pop(stack)
            assert isinstance(fn_ptr, str), "Function pointer type"
            start = fn_ptr.index("[") + 1
            end = fn_ptr.index("]", start)
            return Signature.from_str(fn_ptr[start:end])
        case OpKind.IDENTIFIER:
            raise AssertionError("Identifiers should be resolved by the parser")
        case OpKind.IF_CONDITION:
            return Signature(parameters=["bool"], return_types=[])
        case OpKind.IF_ELIF:
            return Signature(parameters=[], return_types=[])
        case OpKind.IF_ELSE:
            return Signature(parameters=[], return_types=[])
        case OpKind.IF_END:
            return Signature(parameters=[], return_types=[])
        case OpKind.IF_START:
            return Signature(parameters=[], return_types=[])
        case OpKind.LOAD:
            return Signature(parameters=["ptr"], return_types=["any"])
        case OpKind.NOT:
            return Signature(parameters=["any"], return_types=["bool"])
        case OpKind.OVER:
            t1 = GenericType("T1")
            t2 = GenericType("T2")
            return Signature(parameters=[t1, t2], return_types=[t2, t1, t2])
        case OpKind.PUSH_LIST:
            list_type = get_list_literal_type(op)
            return Signature(parameters=[], return_types=[list_type])
        case OpKind.PRINT:
            return Signature(parameters=["any"], return_types=[])
        case OpKind.PUSH_FN:
            assert isinstance(op.value, str), "Expected identifier name"
            function_name = op.value
            assert isinstance(function_name, str), "Expected function name"
            function = GLOBAL_FUNCTIONS.get(function_name)
            assert isinstance(function, Function), "Expected function"

            signature = infer_signature(function.ops)
            return Signature(parameters=[], return_types=[f"fn[{str(signature)}]"])
        case OpKind.PUSH_INT:
            return Signature(parameters=[], return_types=["int"])
        case OpKind.PUSH_STR:
            return Signature(parameters=[], return_types=["str"])
        case OpKind.PUSH_VARIABLE:
            variable_name = op.value
            assert isinstance(op.value, str), "Expected variable name"

            # Global variable
            global_variable = GLOBAL_VARIABLES.get(variable_name)
            if global_variable:
                assert global_variable.typ, "Global variable type should be defined"
                return Signature(parameters=[], return_types=[global_variable.typ])

            # Local variable
            assert isinstance(function, Function), "Expected function"
            for variable in function.variables:
                if variable == variable_name:
                    if not variable.typ:
                        raise AssertionError(
                            f"Variable `{variable.name}` has not been type checked before its usage"
                        )
                    return Signature(parameters=[], return_types=[variable.typ])
            raise NameError(
                f"Function `{function.name}` does not have variable `{variable_name}`"
            )
        case OpKind.ROT:
            t1 = GenericType("T1")
            t2 = GenericType("T2")
            t3 = GenericType("T3")
            return Signature(parameters=[t1, t2, t3], return_types=[t2, t1, t3])
        case OpKind.STORE:
            return Signature(parameters=["ptr", "any"], return_types=[])
        case OpKind.SWAP:
            t1 = GenericType("T1")
            t2 = GenericType("T2")
            return Signature(parameters=[t1, t2], return_types=[t1, t2])
        case OpKind.WHILE_CONDITION:
            return Signature(parameters=["bool"], return_types=[])
        case OpKind.WHILE_END:
            return Signature(parameters=[], return_types=[])
        case OpKind.WHILE_START:
            return Signature(parameters=[], return_types=[])
        case _:
            assert_never(op.kind)


def type_check_ops(ops: list[Op], function: Function | None = None):
    generics: dict[GenericType, Type] = {}
    stack = []
    if function and function.signature:
        for param in reversed(function.signature.parameters):
            stack.append(param)

    for op in ops:
        signature = get_signature_from_op(op, stack, function)
        apply_signature_check(signature, stack, generics)

    if function and function.signature and stack != function.signature.return_types:
        raise TypeError(
            f"""Invalid function signature for function: {function.name}

Signature: {function.signature}
Expected:  {Signature(function.signature.parameters, stack)}"""
        )


def infer_signature(ops: list[Op]) -> Signature:
    parameters: list[Type] = []
    stack: list[Type] = []
    generics: dict[GenericType, Type] = {}

    for op in ops:
        signature = get_signature_from_op(op, stack)
        apply_signature_infer(signature, stack, generics, parameters)

    return Signature(parameters, stack)


def stack_push(stack: list[Type], typ: Type):
    stack.append(typ)


def stack_peek(stack: list[Type]) -> Type | None:
    if not stack:
        return None
    return stack[-1]


def stack_pop(stack: list[Type]) -> Type:
    if not stack:
        raise IndexError("Stack underflow")
    return stack.pop()


def expect_type(stack: list[Type], expected: Type) -> Type:
    typ = stack_pop(stack)
    if expected == "any" or typ == "any":
        return typ
    if typ != expected:
        raise TypeError(f"Expected `{expected}` but got `{typ}`")
    return typ


def get_literal_type(op: Op) -> Type | None:
    match op.kind:
        case OpKind.PUSH_INT:
            return "int"
        case OpKind.PUSH_LIST:
            return "list"
        case _:
            return None


def get_list_literal_type(op: Op) -> Type:
    list_items = op.value
    assert isinstance(list_items, list), "Expected list"

    if not list_items:
        return "list"

    item_type = get_literal_type(list_items[0])
    assert isinstance(item_type, str), "Expected non-generic type"

    return f"list[{item_type}]"
