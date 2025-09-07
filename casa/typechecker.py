from typing import assert_never

from casa.common import GLOBAL_IDENTIFIERS, Function, GenericType, Op, OpKind, Signature, Type


def apply_signature_check(
    signature: Signature,
    stack: list[Type],
    generics: dict[GenericType, Type],
):
    for expected in reversed(signature.parameters):
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
    for expected in reversed(signature.parameters):
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


def get_signature_from_op(op: Op, stack: list[Type]) -> Signature:
    assert len(OpKind) == 16, "Exhaustive handling for `OpKind`"

    match op.kind:
        case OpKind.ADD:
            return Signature(parameters=["int", "int"], return_types=["int"])
        case OpKind.CALL_FN:
            assert isinstance(op.value, str), "Expected identifier name"
            function_name = op.value
            function = GLOBAL_IDENTIFIERS.get(function_name)

            assert isinstance(function, Function), "Expected function"
            assert isinstance(function.signature, Signature), "Expected function signature"

            return function.signature
        case OpKind.DROP:
            return Signature(parameters=["any"], return_types=[])
        case OpKind.DUP:
            t1 = GenericType("T1")
            return Signature(parameters=[t1], return_types=[t1, t1])
        case OpKind.EXEC_FN:
            if not stack:
                raise IndexError("Stack underflow")
            fn_ptr = stack_pop(stack)
            assert isinstance(fn_ptr, str), "Function pointer type"
            start = fn_ptr.index("[") + 1
            end = fn_ptr.index("]", start)
            return Signature.from_str(fn_ptr[start:end])
        case OpKind.LOAD:
            return Signature(parameters=["ptr"], return_types=["any"])
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
            function = GLOBAL_IDENTIFIERS.get(function_name)
            assert isinstance(function, Function), "Expected function"

            signature = infer_signature(function.ops)
            return Signature(parameters=[], return_types=[f"fn[{str(signature)}]"])
        case OpKind.PUSH_INT:
            return Signature(parameters=[], return_types=["int"])
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
        case OpKind.IDENTIFIER:
            raise AssertionError("Identifiers should be resolved by the parser")
        case _:
            assert_never(op.kind)


def type_check_ops(ops: list[Op], stack: list[Type] | None = None):
    if not stack:
        stack = []
    generics: dict[GenericType, Type] = {}

    for op in ops:
        signature = get_signature_from_op(op, stack)
        apply_signature_check(signature, stack, generics)


def infer_signature(ops: list[Op]) -> Signature:
    parameters: list[Type] = []
    stack: list[Type] = []
    generics: dict[GenericType, Type] = {}

    for op in ops:
        signature = get_signature_from_op(op, stack)
        apply_signature_infer(signature, stack, generics, parameters)

    return Signature(parameters, stack)


def get_op_stack_effect(op: Op) -> Signature:
    assert len(OpKind) == 15, "Exhaustive handling for `OpKind`"

    match op.kind:
        case OpKind.ADD:
            return Signature(parameters=["int", "int"], return_types=["int"])
        case OpKind.CALL_FN:
            assert isinstance(op.value, str), "Expected identifier name"
            function_name = op.value
            function = GLOBAL_IDENTIFIERS.get(function_name)

            assert isinstance(function, Function), "Expected function"
            assert isinstance(function.signature, Signature), "Expected function signature"

            return function.signature
        case OpKind.DROP:
            return Signature(parameters=["any"], return_types=[])
        case OpKind.DUP:
            t1 = GenericType("T1")
            return Signature(parameters=[t1], return_types=[t1, t1])
        case OpKind.EXEC_FN:
            return Signature(parameters=["fn"], return_types=[])
        case OpKind.LOAD:
            return Signature(parameters=["ptr"], return_types=["any"])
        case OpKind.OVER:
            t1 = GenericType("T1")
            t2 = GenericType("T2")
            return Signature(parameters=[t1, t2], return_types=[t2, t1, t2])
        case OpKind.PUSH_LIST:
            # TODO: Better typing for lists
            return Signature(parameters=[], return_types=["ptr"])
        case OpKind.PRINT:
            return Signature(parameters=["any"], return_types=[])
        case OpKind.PUSH_FN:
            assert isinstance(op.value, str), "Expected identifier name"
            function_name = op.value
            function = GLOBAL_IDENTIFIERS.get(function_name)
            assert isinstance(function, Function), "Expected function"

            signature = infer_signature(function.ops)
            return Signature(parameters=[], return_types=[f"fn[{str(signature)}]"])
        case OpKind.PUSH_INT:
            return Signature(parameters=[], return_types=["int"])
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
        case OpKind.IDENTIFIER:
            raise AssertionError("Identifiers should be resolved by the parser")
        case _:
            assert_never(op.kind)


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
