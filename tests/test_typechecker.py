"""Tests for casa/typechecker.py — verifies stack effects and op mutations."""

import pytest

from casa.common import ANY_TYPE, GLOBAL_FUNCTIONS, OpKind, Parameter, Signature
from casa.error import WARNINGS, CasaErrorCollection, ErrorKind, WarningKind
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
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    assert exc_info.value.errors[0].kind == ErrorKind.SIGNATURE_MISMATCH


def test_typecheck_fn_unused_param_passthrough():
    """Unnamed params pass through the stack, producing a warning."""
    code = "fn foo int -> int {} 5 foo"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]
    assert len(WARNINGS) == 1
    assert WARNINGS[0].kind == WarningKind.UNUSED_PARAMETER
    assert "foo" in WARNINGS[0].message


def test_typecheck_fn_unused_param_no_warning_when_used():
    """Named params that are used produce no warning."""
    code = "fn foo a:int -> int { a } 5 foo"
    typecheck_string(code)
    assert len(WARNINGS) == 0


def test_typecheck_fn_unused_param_wrong_return_type():
    """Passthrough with wrong return type is still an error."""
    code = "fn bad int -> str {} bad"
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    assert exc_info.value.errors[0].kind == ErrorKind.SIGNATURE_MISMATCH


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
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    error = exc_info.value.errors[0]
    assert error.kind == ErrorKind.STACK_MISMATCH
    assert "incompatible stack effects" in error.message
    assert len(error.notes) == 2
    labels = [n[0] for n in error.notes]
    assert any("`if` branch" in l for l in labels)
    assert any("`else` branch" in l for l in labels)


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


# ---------------------------------------------------------------------------
# Generics (type variables)
# ---------------------------------------------------------------------------
def test_typecheck_generic_identity_int():
    code = "fn id[T] T -> T { } 42 id"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_generic_identity_str():
    code = 'fn id[T] T -> T { } "hello" id'
    sig = typecheck_string(code)
    assert sig.return_types == ["str"]


def test_typecheck_generic_identity_struct():
    code = """
    struct Point { x: int y: int }
    fn id[T] T -> T { }
    1 2 Point id
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["Point"]


def test_typecheck_generic_swap():
    code = 'fn swap_t[T1 T2] T1 T2 -> T1 T2 { swap } 5 "hi" swap_t'
    sig = typecheck_string(code)
    assert sig.return_types == ["str", "int"]


def test_typecheck_generic_named_params():
    code = "fn first[T1 T2] a:T1 b:T2 -> T1 { a } 42 99 first"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_generic_mixed_concrete_and_type_var():
    code = "fn wrap[T] T -> T int { 42 } 5 wrap"
    sig = typecheck_string(code)
    assert sig.return_types == ["int", "int"]


def test_typecheck_generic_consistency_error():
    code = 'fn pair[T] T T -> T T { } 42 "hi" pair'
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH
    assert "bound to" in exc_info.value.errors[0].message


def test_typecheck_generic_return_only_type_var_error():
    code = "fn bad[T] int -> T { } 42 bad"
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH
    assert "return types but not in parameters" in exc_info.value.errors[0].message


def test_typecheck_generic_called_from_function():
    code = """
    fn id[T] T -> T { }
    fn double a:int -> int { a id 2 * }
    5 double
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_generic_definition_without_call():
    code = "fn id[T] T -> T { }"
    typecheck_string(code)  # Should not raise


def test_typecheck_generic_in_impl_block():
    code = """
    struct Box { val: int }
    impl Box {
        fn apply[T] self:Box T -> T { }
    }
    42 99 Box .apply
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_generic_lambda_wrapping():
    code = """
    fn id[T] T -> T { }
    42 { id } exec
    """
    sig = typecheck_string(code)
    # Lambda exec produces `any` — generic type info is lost through lambda
    assert sig.return_types == ["any"]


# ---------------------------------------------------------------------------
# type_check_functions
# ---------------------------------------------------------------------------
def test_typecheck_mismatch_shows_origin():
    """TYPE_MISMATCH error includes a note pointing to where the value was pushed."""
    code = '"hello" 1 *'
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    error = exc_info.value.errors[0]
    assert error.kind == ErrorKind.TYPE_MISMATCH
    assert "`*`" in error.message
    assert "int int -> int" in error.message
    assert len(error.notes) == 1
    note_message, note_location = error.notes[0]
    assert "str" in note_message
    assert note_location.span.offset == 0  # points to "hello"


def test_typecheck_mismatch_shows_fn_parameter_context():
    """TYPE_MISMATCH error shows which parameter mismatched."""
    code = "fn foo a:str -> str { a } 42 foo"
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    error = exc_info.value.errors[0]
    assert error.kind == ErrorKind.TYPE_MISMATCH
    assert "`foo`" in error.message
    assert "str -> str" in error.message
    assert "parameter 1 of `foo`" in error.expected


def test_typecheck_all_functions_catches_unused_error():
    """An unused function with a type error is caught."""
    code = "fn bad int -> str { }"
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    error = exc_info.value.errors[0]
    assert error.kind == ErrorKind.SIGNATURE_MISMATCH
    assert error.expected == "int -> str"
    assert error.got == "None -> None"
    assert error.got_label == "Inferred"


def test_typecheck_all_functions_valid_unused():
    """A valid unused function does not raise."""
    code = "fn add a:int b:int -> int { a b + }"
    typecheck_string(code)  # Should not raise
    fn = GLOBAL_FUNCTIONS["add"]
    assert fn.is_typechecked


def test_typecheck_all_functions_with_struct():
    """Auto-generated struct getters/setters typecheck correctly."""
    code = """
    struct Point { x: int y: int }
    fn get_x p:Point -> int { p.x }
    """
    typecheck_string(code)  # Should not raise
    fn = GLOBAL_FUNCTIONS["get_x"]
    assert fn.is_typechecked


def test_typecheck_if_elif_inconsistent_raises():
    """Three-branch if/elif/else with incompatible stacks shows all branches."""
    code = "if true then 1 elif false then 2 3 else 4 fi"
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    error = exc_info.value.errors[0]
    assert error.kind == ErrorKind.STACK_MISMATCH
    assert "incompatible stack effects" in error.message
    labels = [n[0] for n in error.notes]
    assert any("`if` branch" in l for l in labels)
    assert any("`elif` branch" in l for l in labels)


def test_typecheck_if_no_else_mismatch_raises():
    """An if-without-else that changes the stack is a mismatch."""
    code = "if true then 1 fi"
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    error = exc_info.value.errors[0]
    assert error.kind == ErrorKind.STACK_MISMATCH
    assert "incompatible stack effects" in error.message
    labels = [n[0] for n in error.notes]
    assert any("`if` branch" in l for l in labels)


def test_format_branch_signature():
    from casa.typechecker import _format_branch_signature

    assert _format_branch_signature([], ["int"]) == "`None -> int`"
    assert _format_branch_signature(["int"], ["int", "str"]) == "`int -> int str`"
    assert _format_branch_signature([], []) == "`None -> None`"
    assert _format_branch_signature(["any"], ["any", "int"]) == "`any -> any int`"
