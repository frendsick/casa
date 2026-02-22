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
        ("[1, 2]", "array[int]"),
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
# Syscall intrinsics
# ---------------------------------------------------------------------------
def test_typecheck_syscall0():
    sig = typecheck_string("60 syscall0")
    assert sig.return_types == ["int"]


def test_typecheck_syscall1():
    sig = typecheck_string("0 60 syscall1")
    assert sig.return_types == ["int"]


def test_typecheck_syscall2():
    sig = typecheck_string("0 0 60 syscall2")
    assert sig.return_types == ["int"]


def test_typecheck_syscall3():
    sig = typecheck_string("1 0 1 1 syscall3")
    assert sig.return_types == ["int"]


def test_typecheck_syscall4():
    sig = typecheck_string("0 0 0 0 60 syscall4")
    assert sig.return_types == ["int"]


def test_typecheck_syscall5():
    sig = typecheck_string("0 0 0 0 0 60 syscall5")
    assert sig.return_types == ["int"]


def test_typecheck_syscall6():
    sig = typecheck_string("1 2 3 4 5 6 100 syscall6")
    assert sig.return_types == ["int"]


def test_typecheck_syscall_accepts_any_type_for_args():
    """Syscall arguments (not the syscall number) accept any type."""
    sig = typecheck_string('"hello" 42 syscall1')
    assert sig.return_types == ["int"]


@pytest.mark.parametrize(
    "code",
    [
        ('"not_a_number" syscall0'),
        ('0 "not_a_number" syscall1'),
        ('0 0 "not_a_number" syscall2'),
        ('0 0 0 "not_a_number" syscall3'),
        ('0 0 0 0 "not_a_number" syscall4'),
        ('0 0 0 0 0 "not_a_number" syscall5'),
        ('0 0 0 0 0 0 "not_a_number" syscall6'),
    ],
)
def test_typecheck_syscall_requires_int_syscall_number(code):
    """Syscall number (top of stack) must be int for all syscall intrinsics."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(code)
    assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH


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


def test_typecheck_local_shadows_global():
    """Function parameter with same name as global variable shadows it."""
    code = '"hello" = x fn foo x:int -> int { x } 42 foo'
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
# Function references
# ---------------------------------------------------------------------------
def test_typecheck_fn_ref_pushes_signature():
    code = "fn add a:int b:int -> int { a b + } &add"
    sig = typecheck_string(code)
    assert sig.return_types == ["fn[int int -> int]"]


def test_typecheck_fn_ref_exec():
    code = "fn inc a:int -> int { a 1 + } 5 &inc exec"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_fn_ref_method():
    code = """
    struct Point { x: int y: int }
    impl Point {
        fn sum self:Point -> int { self Point::x self Point::y + }
    }
    &Point::sum
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["fn[Point -> int]"]


def test_typecheck_fn_ref_accessor():
    code = """
    struct Point { x: int y: int }
    &Point::x
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["fn[Point -> int]"]


def test_typecheck_fn_ref_inside_function():
    code = """
    fn inc a:int -> int { a 1 + }
    fn apply_inc x:int -> int { x &inc exec }
    5 apply_inc
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


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


# ---------------------------------------------------------------------------
# F-strings
# ---------------------------------------------------------------------------
def test_typecheck_fstring_plain_text():
    """f"hello" has stack effect -> str."""
    sig = typecheck_string('f"hello"')
    assert sig.return_types == ["str"]


def test_typecheck_fstring_with_str_expression():
    """f-string with expression returning str typechecks correctly."""
    sig = typecheck_string('"world" = x f"hello {x}"')
    assert sig.return_types == ["str"]


def test_typecheck_fstring_single_expression_keeps_type():
    """f-string with only a single expression and no text parts returns the expression type.

    When there is only one part (no FSTRING_CONCAT), the type is whatever
    the expression lambda returns. For a typed global int, exec returns int.
    """
    sig = typecheck_string('42 = x f"{x}"')
    assert sig.return_types == ["int"]


def test_typecheck_fstring_int_expression_with_text_raises():
    """f-string expression returning int combined with text parts raises type error.

    When FSTRING_CONCAT is emitted (text + expression), all parts must be str.
    An int expression causes a TYPE_MISMATCH.
    """
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string('42 = x f"val: {x}"')
    error = exc_info.value.errors[0]
    assert error.kind == ErrorKind.TYPE_MISMATCH


def test_typecheck_fstring_multiple_expressions():
    """f-string with multiple str expressions typechecks correctly."""
    sig = typecheck_string('"a" = x "b" = y f"{x} and {y}"')
    assert sig.return_types == ["str"]


def test_typecheck_fstring_concat_pushes_str():
    """FSTRING_CONCAT pops N strings and pushes one str."""
    sig = typecheck_string('"a" = x f"prefix {x} suffix"')
    assert sig.return_types == ["str"]


def test_typecheck_fstring_in_function():
    """f-strings work inside function bodies with correct type."""
    code = """
    fn greet name:str -> str { f"hello {name}" }
    "world" greet
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["str"]


# ---------------------------------------------------------------------------
# to_str methods (standard library)
# ---------------------------------------------------------------------------
STD_INCLUDE = 'include "lib/std.casa"\n'


def test_typecheck_int_to_str():
    """int::to_str returns str."""
    sig = typecheck_string(STD_INCLUDE + "42.to_str")
    assert sig.return_types == ["str"]


def test_typecheck_int_to_str_negative():
    """int::to_str works with negative integers."""
    sig = typecheck_string(STD_INCLUDE + "-42.to_str")
    assert sig.return_types == ["str"]


def test_typecheck_int_to_str_zero():
    """int::to_str works with zero."""
    sig = typecheck_string(STD_INCLUDE + "0.to_str")
    assert sig.return_types == ["str"]


def test_typecheck_bool_to_str():
    """bool::to_str returns str."""
    sig = typecheck_string(STD_INCLUDE + "true.to_str")
    assert sig.return_types == ["str"]


def test_typecheck_bool_to_str_false():
    """bool::to_str returns str for false."""
    sig = typecheck_string(STD_INCLUDE + "false.to_str")
    assert sig.return_types == ["str"]


def test_typecheck_str_to_str():
    """str::to_str returns str (identity)."""
    sig = typecheck_string(STD_INCLUDE + '"hello".to_str')
    assert sig.return_types == ["str"]


def test_typecheck_ptr_to_str():
    """ptr::to_str returns str."""
    sig = typecheck_string(STD_INCLUDE + "10 alloc .to_str")
    assert sig.return_types == ["str"]


def test_typecheck_to_str_in_fstring():
    """to_str can be used inside f-strings to convert int to str."""
    sig = typecheck_string(STD_INCLUDE + '42 = n f"val: {n.to_str}"')
    assert sig.return_types == ["str"]


def test_typecheck_to_str_fn_signature():
    """int::to_str has correct inferred signature."""
    typecheck_string(STD_INCLUDE + "42.to_str")
    fn = GLOBAL_FUNCTIONS["int::to_str"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == ["int"]
    assert fn.signature.return_types == ["str"]


# ---------------------------------------------------------------------------
# Typed array literals
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "code,expected_type",
    [
        ("[1, 2, 3]", "array[int]"),
        ('["hello", "world"]', "array[str]"),
        ("[true, false]", "array[bool]"),
    ],
)
def test_typecheck_typed_array_literal(code, expected_type):
    """Array literals infer element type from their contents."""
    sig = typecheck_string(code)
    assert sig.return_types == [expected_type]


def test_typecheck_empty_array_is_array_any():
    """Empty array literal produces array[any]."""
    sig = typecheck_string("[]")
    assert sig.return_types == ["array[any]"]


def test_typecheck_nested_array():
    """Nested array literal produces array[array[int]]."""
    sig = typecheck_string("[[1, 2], [3, 4]]")
    assert sig.return_types == ["array[array[int]]"]


def test_typecheck_heterogeneous_array_raises():
    """Mixed-type array literal raises TYPE_MISMATCH."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string('[1, "hello"]')
    assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH


def test_typecheck_bare_array_matches_typed_array():
    """A function expecting bare `array` accepts `array[int]` (backward compat)."""
    code = """
    fn first arr:array -> int { 0 }
    [1, 2, 3] first
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_typed_array_matches_bare_array():
    """A function expecting `array[int]` accepts bare `array` (backward compat)."""
    code = """
    fn first arr:array[int] -> int { 0 }
    """
    typecheck_string(code)
    fn = GLOBAL_FUNCTIONS["first"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == ["array[int]"]


def test_typecheck_signature_matches_bare_and_typed_array():
    """Signature.matches() treats bare `array` as compatible with `array[int]`."""
    sig_bare = Signature([Parameter("array")], ["int"])
    sig_typed = Signature([Parameter("array[int]")], ["int"])
    assert sig_bare.matches(sig_typed)
    assert sig_typed.matches(sig_bare)


def test_typecheck_signature_matches_different_typed_arrays():
    """Signature.matches() rejects array[int] vs array[str]."""
    sig_int = Signature([Parameter("array[int]")], ["int"])
    sig_str = Signature([Parameter("array[str]")], ["int"])
    assert not sig_int.matches(sig_str)
    assert not sig_str.matches(sig_int)


def test_typecheck_parameterized_type_in_fn_signature():
    """Parser handles parameterized types in function signatures."""
    code = """
    fn sum_array arr:array[int] -> int { 0 }
    [1, 2, 3] sum_array
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]
    fn = GLOBAL_FUNCTIONS["sum_array"]
    assert [p.typ for p in fn.signature.parameters] == ["array[int]"]


def test_typecheck_type_cast_typed_array():
    """Type cast (array[int]) works."""
    code = "[] (array[int])"
    sig = typecheck_string(code)
    assert sig.return_types == ["array[int]"]


def test_typecheck_array_dup():
    """dup on typed array preserves the element type."""
    sig = typecheck_string("[1, 2] dup")
    assert sig.return_types == ["array[int]", "array[int]"]


def test_typecheck_array_in_variable():
    """Typed array stored in variable retains its type."""
    code = "[1, 2, 3] = nums nums"
    sig = typecheck_string(code)
    assert sig.return_types == ["array[int]"]


def test_typecheck_array_nth_returns_element_type():
    """array::nth on a typed array returns the element type, not any."""
    code = STD_INCLUDE + "0 [1, 2, 3] array::nth"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_array_with_int_variables():
    """Array literal with int variables produces array[int]."""
    code = "42 = x [x, 2, 3]"
    sig = typecheck_string(code)
    assert sig.return_types == ["array[int]"]


def test_typecheck_array_with_str_variables():
    """Array literal with str variables produces array[str]."""
    code = '"a" = x "b" = y [x, y]'
    sig = typecheck_string(code)
    assert sig.return_types == ["array[str]"]


def test_typecheck_array_mixed_variable_and_literal_raises():
    """Array with variable and literal of different types raises TYPE_MISMATCH."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string('42 = x [x, "hello"]')
    assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH


def test_typecheck_array_with_global_variable():
    """Array literal with global variable infers correct type."""
    code = "10 = g [g, 20, 30]"
    sig = typecheck_string(code)
    assert sig.return_types == ["array[int]"]


# ---------------------------------------------------------------------------
# fn[sig] as parameter type (type checker)
# ---------------------------------------------------------------------------
def test_typecheck_fn_type_parameter():
    """Function with fn[int -> int] parameter type checks correctly."""
    code = """
    fn apply f:fn[int -> int] x:int -> int { x f exec }
    5 { 1 + } apply
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_generic_fn_type_binding():
    """Generic fn type parameter binds type variables correctly."""
    code = """
    fn apply[T] f:fn[T -> T] x:T -> T { x f exec }
    5 { 1 + } apply
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_generic_fn_type_two_vars():
    """Generic fn type with T1 and T2 binds correctly."""
    code = """
    fn transform[T1 T2] f:fn[T1 -> T2] x:T1 -> T2 { x f exec }
    42 { 0 > } transform
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["bool"]


def test_typecheck_fn_type_records_in_signature():
    """fn[int -> int] is properly recorded in the function signature."""
    code = "fn apply f:fn[int -> int] x:int -> int { x f exec }"
    typecheck_string(code)
    fn = GLOBAL_FUNCTIONS["apply"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == ["fn[int -> int]", "int"]
    assert fn.signature.return_types == ["int"]


# ---------------------------------------------------------------------------
# Signature.from_str with fn types
# ---------------------------------------------------------------------------
def test_signature_from_str_fn_type():
    """Signature.from_str handles fn[int -> int] as a parameter type."""
    sig = Signature.from_str("fn[int -> int] int -> int")
    assert [p.typ for p in sig.parameters] == ["fn[int -> int]", "int"]
    assert sig.return_types == ["int"]


def test_signature_from_str_fn_type_nested():
    """Signature.from_str handles fn[array[int] -> int] with nested brackets."""
    sig = Signature.from_str("fn[array[int] -> int] -> int")
    assert [p.typ for p in sig.parameters] == ["fn[array[int] -> int]"]
    assert sig.return_types == ["int"]


def test_signature_from_str_fn_type_multi_param():
    """Signature.from_str handles fn[int int -> int] with multiple params."""
    sig = Signature.from_str("fn[int int -> int] int int -> int")
    assert [p.typ for p in sig.parameters] == ["fn[int int -> int]", "int", "int"]
    assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# map / filter / reduce (type checker)
# ---------------------------------------------------------------------------
def test_typecheck_map_returns_array_int():
    """map with fn[int -> int] on array[int] returns array[int]."""
    code = STD_INCLUDE + "{ 2 * } [1, 2, 3] .map"
    sig = typecheck_string(code)
    assert sig.return_types == ["array[int]"]


def test_typecheck_map_type_transform():
    """map with fn[int -> bool] on array[int] returns array[bool]."""
    code = STD_INCLUDE + "{ 0 > } [1, 2, 3] .map"
    sig = typecheck_string(code)
    assert sig.return_types == ["array[bool]"]


def test_typecheck_filter_returns_array_int():
    """filter with fn[int -> bool] on array[int] returns array[int]."""
    code = STD_INCLUDE + "{ 1 > } [1, 2, 3] .filter"
    sig = typecheck_string(code)
    assert sig.return_types == ["array[int]"]


def test_typecheck_reduce_returns_int():
    """reduce with fn[int int -> int] on array[int] returns int."""
    code = STD_INCLUDE + "{ + } 0 [1, 2, 3] .reduce"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_map_then_filter():
    """Chaining map then filter preserves array type."""
    code = STD_INCLUDE + """
    { 2 * } [1, 2, 3] .map = mapped
    { 4 > } mapped .filter
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["array[int]"]


def test_typecheck_map_str_array():
    """map on array[str] with any->any identity returns array[any]."""
    code = STD_INCLUDE + '{ dup drop } ["a", "b", "c"] .map'
    sig = typecheck_string(code)
    assert sig.return_types == ["array[any]"]
