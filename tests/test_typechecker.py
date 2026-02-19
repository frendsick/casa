"""Tests for casa/typechecker.py — verifies stack effects and op mutations."""

import pytest

from casa.common import ANY_TYPE, GLOBAL_FUNCTIONS, OpKind, Parameter, Signature
from tests.conftest import parse_string, resolve_string, typecheck_string


# ---------------------------------------------------------------------------
# Literals
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "code,expected_type",
    [
        ("42", "int"),
        ("-42", "int"),
        ("true", "bool"),
        ('"hello"', "str"),
        ("[1, 2]", "array"),
    ],
)
def test_typecheck_literals(code, expected_type):
    sig = typecheck_string(code)
    assert sig.return_types == [expected_type]


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("op", ["+", "-", "*", "/", "%"])
def test_typecheck_arithmetic(op):
    sig = typecheck_string(f"1 2 {op}")
    assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# Bitshift
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("op", ["<<", ">>"])
def test_typecheck_bitshift(op):
    sig = typecheck_string(f"8 2 {op}")
    assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# Boolean
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("op", ["&&", "||"])
def test_typecheck_boolean_binary(op):
    sig = typecheck_string(f"true false {op}")
    assert sig.return_types == ["bool"]


def test_typecheck_boolean_not():
    sig = typecheck_string("true !")
    assert sig.return_types == ["bool"]


# ---------------------------------------------------------------------------
# Comparison
# ---------------------------------------------------------------------------
@pytest.mark.parametrize("op", ["==", "!=", "<", "<=", ">", ">="])
def test_typecheck_comparison(op):
    sig = typecheck_string(f"1 2 {op}")
    assert sig.return_types == ["bool"]


# ---------------------------------------------------------------------------
# Stack intrinsics
# ---------------------------------------------------------------------------
def test_typecheck_drop():
    sig = typecheck_string("42 drop")
    assert sig.return_types == []


def test_typecheck_dup():
    sig = typecheck_string("42 dup")
    assert sig.return_types == ["int", "int"]


def test_typecheck_swap():
    sig = typecheck_string('42 "hi" swap')
    assert sig.return_types == ["str", "int"]


def test_typecheck_over():
    sig = typecheck_string('42 "hi" over')
    assert sig.return_types == ["int", "str", "int"]


def test_typecheck_rot():
    sig = typecheck_string('1 "a" true rot')
    assert sig.return_types == ["str", "bool", "int"]


# ---------------------------------------------------------------------------
# Memory intrinsics
# ---------------------------------------------------------------------------
def test_typecheck_alloc():
    sig = typecheck_string("10 alloc")
    assert sig.return_types == ["ptr"]


def test_typecheck_load():
    sig = typecheck_string("10 alloc load")
    assert sig.return_types == [ANY_TYPE]


def test_typecheck_store():
    sig = typecheck_string("42 10 alloc store")
    assert sig.return_types == []


# ---------------------------------------------------------------------------
# Print resolves to PRINT_INT / PRINT_STR
# ---------------------------------------------------------------------------
def test_typecheck_print_resolves_to_print_int():
    ops = resolve_string("42 print")
    from casa.typechecker import type_check_ops

    type_check_ops(ops)
    print_op = [o for o in ops if o.kind in (OpKind.PRINT_INT, OpKind.PRINT_STR)][0]
    assert print_op.kind == OpKind.PRINT_INT


def test_typecheck_print_resolves_to_print_str():
    ops = resolve_string('"hello" print')
    from casa.typechecker import type_check_ops

    type_check_ops(ops)
    print_op = [o for o in ops if o.kind in (OpKind.PRINT_INT, OpKind.PRINT_STR)][0]
    assert print_op.kind == OpKind.PRINT_STR


# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------
def test_typecheck_fn_signature_inference():
    code = "fn add a:int b:int -> int { a b + }"
    typecheck_string(code)
    fn = GLOBAL_FUNCTIONS["add"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == ["int", "int"]
    assert fn.signature.return_types == ["int"]


def test_typecheck_fn_signature_mismatch():
    # The mismatch is only detected when the function is called
    code = "fn bad a:int -> str { a 1 + } bad"
    with pytest.raises(TypeError, match="Invalid signature"):
        typecheck_string(code)


def test_typecheck_fn_call_stack_effect():
    code = "fn double a:int -> int { a 2 * } 5 double"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# Method call resolution
# ---------------------------------------------------------------------------
def test_typecheck_method_call_resolution():
    code = """
    struct Pair { x: int y: int }
    42 99 Pair .x
    """
    ops = resolve_string(code)
    from casa.typechecker import type_check_ops

    type_check_ops(ops)
    # METHOD_CALL should be mutated to FN_CALL
    fn_calls = [o for o in ops if o.kind == OpKind.FN_CALL]
    method_calls = [o for o in ops if o.kind == OpKind.METHOD_CALL]
    assert len(fn_calls) >= 1
    assert any("Pair::x" in str(o.value) for o in fn_calls)
    assert len(method_calls) == 0


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------
def test_typecheck_if_else_consistent():
    code = "if true then 1 else 2 fi"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_if_inconsistent_raises():
    code = 'if true then 1 else "two" fi'
    with pytest.raises(TypeError):
        typecheck_string(code)


def test_typecheck_while_loop():
    code = "while false do done"
    sig = typecheck_string(code)
    assert sig.return_types == []


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------
def test_typecheck_variable_assign_and_push():
    code = "42 = x x"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# Type cast
# ---------------------------------------------------------------------------
def test_typecheck_type_cast():
    code = "42 (str)"
    sig = typecheck_string(code)
    assert sig.return_types == ["str"]


# ---------------------------------------------------------------------------
# Lambdas
# ---------------------------------------------------------------------------
def test_typecheck_lambda_push():
    code = "{ 1 + }"
    sig = typecheck_string(code)
    assert len(sig.return_types) == 1
    assert sig.return_types[0].startswith("fn[")


def test_typecheck_lambda_exec():
    # Lambda captures from other scopes are typed as `any`
    code = "42 { 1 + } exec"
    sig = typecheck_string(code)
    assert sig.return_types == ["any"]


# ---------------------------------------------------------------------------
# ANY_TYPE matching
# ---------------------------------------------------------------------------
def test_typecheck_any_type_matches():
    sig_a = Signature([Parameter("int")], ["int"])
    sig_b = Signature([Parameter(ANY_TYPE)], [ANY_TYPE])
    assert sig_a.matches(sig_b)
    assert sig_b.matches(sig_a)


def test_typecheck_any_type_no_match_different_length():
    sig_a = Signature([Parameter("int")], ["int"])
    sig_b = Signature([Parameter("int"), Parameter("int")], ["int"])
    assert not sig_a.matches(sig_b)


# ---------------------------------------------------------------------------
# FN_EXEC stack effect
# ---------------------------------------------------------------------------
def test_typecheck_fn_exec():
    code = "fn inc a:int -> int { a 1 + } 5 { inc } exec"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]
