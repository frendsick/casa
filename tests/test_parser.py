"""Tests for casa/parser.py — covers OpKind values via parse_ops and resolve_identifiers."""

import pytest

from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_STRUCTS,
    GLOBAL_VARIABLES,
    Function,
    Intrinsic,
    Keyword,
    Op,
    OpKind,
    Operator,
    Struct,
    Variable,
)
from casa.error import CasaErrorCollection, ErrorKind
from tests.conftest import lex_string, parse_string, resolve_string


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def find_ops(ops: list[Op], kind: OpKind) -> list[Op]:
    return [op for op in ops if op.kind == kind]


# ---------------------------------------------------------------------------
# Literals
# ---------------------------------------------------------------------------
def test_parse_push_int():
    ops = parse_string("42")
    assert len(ops) == 1
    assert ops[0].kind == OpKind.PUSH_INT
    assert ops[0].value == 42


@pytest.mark.parametrize("text,expected", [("true", True), ("false", False)])
def test_parse_push_bool(text, expected):
    ops = parse_string(text)
    assert ops[0].kind == OpKind.PUSH_BOOL
    assert ops[0].value is expected


def test_parse_push_str():
    ops = parse_string('"hello"')
    assert ops[0].kind == OpKind.PUSH_STR
    assert ops[0].value == "hello"


def test_parse_push_str_with_escapes():
    ops = parse_string(r'"hello\nworld"')
    assert ops[0].kind == OpKind.PUSH_STR
    assert ops[0].value == "hello\nworld"


def test_parse_negative_int():
    ops = parse_string("-42")
    assert len(ops) == 1
    assert ops[0].kind == OpKind.PUSH_INT
    assert ops[0].value == -42


def test_parse_negative_int_in_array():
    ops = parse_string("[-1, -2, -3]")
    assert ops[0].kind == OpKind.PUSH_ARRAY
    assert len(ops[0].value) == 3
    assert ops[0].value[0].value == -1
    assert ops[0].value[1].value == -2
    assert ops[0].value[2].value == -3


def test_parse_push_array():
    ops = parse_string("[1, 2, 3]")
    assert ops[0].kind == OpKind.PUSH_ARRAY
    assert len(ops[0].value) == 3


# ---------------------------------------------------------------------------
# Operators
# ---------------------------------------------------------------------------
OPERATOR_CASES = [
    ("+", OpKind.ADD, Operator.PLUS),
    ("-", OpKind.SUB, Operator.MINUS),
    ("*", OpKind.MUL, Operator.MULTIPLICATION),
    ("/", OpKind.DIV, Operator.DIVISION),
    ("%", OpKind.MOD, Operator.MODULO),
    ("<<", OpKind.SHL, Operator.SHL),
    (">>", OpKind.SHR, Operator.SHR),
    ("&&", OpKind.AND, Operator.AND),
    ("||", OpKind.OR, Operator.OR),
    ("!", OpKind.NOT, Operator.NOT),
    ("==", OpKind.EQ, Operator.EQ),
    (">=", OpKind.GE, Operator.GE),
    (">", OpKind.GT, Operator.GT),
    ("<=", OpKind.LE, Operator.LE),
    ("<", OpKind.LT, Operator.LT),
    ("!=", OpKind.NE, Operator.NE),
]


@pytest.mark.parametrize("text,expected_kind,expected_value", OPERATOR_CASES)
def test_parse_all_operators(text, expected_kind, expected_value):
    ops = parse_string(f"1 2 {text}")
    op = [o for o in ops if o.kind == expected_kind][0]
    assert op.value == expected_value


# ---------------------------------------------------------------------------
# Intrinsics
# ---------------------------------------------------------------------------
INTRINSIC_CASES = [
    ("drop", OpKind.DROP, Intrinsic.DROP),
    ("dup", OpKind.DUP, Intrinsic.DUP),
    ("over", OpKind.OVER, Intrinsic.OVER),
    ("rot", OpKind.ROT, Intrinsic.ROT),
    ("swap", OpKind.SWAP, Intrinsic.SWAP),
    ("alloc", OpKind.HEAP_ALLOC, Intrinsic.ALLOC),
    ("load", OpKind.LOAD, Intrinsic.LOAD),
    ("store", OpKind.STORE, Intrinsic.STORE),
    ("print", OpKind.PRINT, Intrinsic.PRINT),
    ("exec", OpKind.FN_EXEC, Intrinsic.EXEC),
]


@pytest.mark.parametrize("text,expected_kind,expected_value", INTRINSIC_CASES)
def test_parse_all_intrinsics(text, expected_kind, expected_value):
    ops = parse_string(text)
    assert ops[0].kind == expected_kind
    assert ops[0].value == expected_value


# ---------------------------------------------------------------------------
# Control flow: if/elif/else/fi
# ---------------------------------------------------------------------------
def test_parse_if_then_fi():
    ops = parse_string("if true then 1 fi")
    kinds = [op.kind for op in ops]
    assert OpKind.IF_START in kinds
    assert OpKind.IF_CONDITION in kinds
    assert OpKind.IF_END in kinds


def test_parse_if_elif_else():
    ops = parse_string("if true then 1 elif false then 2 else 3 fi")
    kinds = [op.kind for op in ops]
    assert OpKind.IF_ELIF in kinds
    assert OpKind.IF_ELSE in kinds


# ---------------------------------------------------------------------------
# Control flow: while/do/done
# ---------------------------------------------------------------------------
def test_parse_while_do_done():
    ops = parse_string("while true do 1 drop done")
    kinds = [op.kind for op in ops]
    assert OpKind.WHILE_START in kinds
    assert OpKind.WHILE_CONDITION in kinds
    assert OpKind.WHILE_END in kinds


def test_parse_while_break_continue():
    ops = parse_string("while true do break continue done")
    kinds = [op.kind for op in ops]
    assert OpKind.WHILE_BREAK in kinds
    assert OpKind.WHILE_CONTINUE in kinds


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------
def test_parse_assign_variable():
    ops = parse_string("42 = x")
    assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
    assert len(assign_ops) == 1
    assert assign_ops[0].value == "x"


def test_parse_assign_increment():
    ops = parse_string("1 += x")
    inc_ops = find_ops(ops, OpKind.ASSIGN_INCREMENT)
    assert len(inc_ops) == 1
    assert inc_ops[0].value == "x"


def test_parse_assign_decrement():
    ops = parse_string("1 -= x")
    dec_ops = find_ops(ops, OpKind.ASSIGN_DECREMENT)
    assert len(dec_ops) == 1
    assert dec_ops[0].value == "x"


# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------
def test_parse_function():
    parse_string("fn add a:int b:int -> int { a b + }")
    assert "add" in GLOBAL_FUNCTIONS
    fn = GLOBAL_FUNCTIONS["add"]
    assert isinstance(fn, Function)
    assert fn.signature is not None
    assert fn.signature.return_types == ["int"]
    # Function body has ops for assigning params + body
    fn_return_ops = find_ops(fn.ops, OpKind.ASSIGN_VARIABLE)
    assert len(fn_return_ops) == 2  # a and b are assigned


def test_parse_function_registers_globally():
    parse_string("fn greet { }")
    assert "greet" in GLOBAL_FUNCTIONS


# ---------------------------------------------------------------------------
# Structs
# ---------------------------------------------------------------------------
def test_parse_struct():
    parse_string("struct Person { name: str age: int }")
    assert "Person" in GLOBAL_STRUCTS
    struct = GLOBAL_STRUCTS["Person"]
    assert isinstance(struct, Struct)
    assert len(struct.members) == 2
    assert struct.members[0].name == "name"
    assert struct.members[1].name == "age"


def test_parse_struct_generates_getters_setters():
    parse_string("struct Point { x: int y: int }")
    assert "Point::x" in GLOBAL_FUNCTIONS
    assert "Point::y" in GLOBAL_FUNCTIONS
    assert "Point::set_x" in GLOBAL_FUNCTIONS
    assert "Point::set_y" in GLOBAL_FUNCTIONS


# ---------------------------------------------------------------------------
# Impl blocks
# ---------------------------------------------------------------------------
def test_parse_impl_block():
    code = """
    struct Foo { val: int }
    impl Foo {
        fn double self:Foo -> int { self Foo::val 2 * }
    }
    """
    parse_string(code)
    assert "Foo::double" in GLOBAL_FUNCTIONS


# ---------------------------------------------------------------------------
# Lambdas
# ---------------------------------------------------------------------------
def test_parse_lambda():
    ops = parse_string("{ 1 + }")
    fn_push_ops = find_ops(ops, OpKind.FN_PUSH)
    assert len(fn_push_ops) == 1
    lambda_name = fn_push_ops[0].value
    assert lambda_name.startswith("lambda__")
    assert lambda_name in GLOBAL_FUNCTIONS


# ---------------------------------------------------------------------------
# Type cast
# ---------------------------------------------------------------------------
def test_parse_type_cast():
    ops = parse_string("42 (str)")
    cast_ops = find_ops(ops, OpKind.TYPE_CAST)
    assert len(cast_ops) == 1
    assert cast_ops[0].value == "str"


# ---------------------------------------------------------------------------
# Dot / Arrow method calls
# ---------------------------------------------------------------------------
def test_parse_dot_method_call():
    # .name produces a METHOD_CALL op
    ops = parse_string("x .name")
    method_ops = find_ops(ops, OpKind.METHOD_CALL)
    assert len(method_ops) == 1
    assert method_ops[0].value == "name"


def test_parse_arrow_setter():
    # ->name produces a METHOD_CALL with set_ prefix
    ops = parse_string("42 x ->name")
    method_ops = find_ops(ops, OpKind.METHOD_CALL)
    assert len(method_ops) == 1
    assert method_ops[0].value == "set_name"


# ---------------------------------------------------------------------------
# Include
# ---------------------------------------------------------------------------
def test_parse_include():
    ops = parse_string('include "some/file.casa"')
    include_ops = find_ops(ops, OpKind.INCLUDE_FILE)
    assert len(include_ops) == 1


# ---------------------------------------------------------------------------
# Identifier resolution
# ---------------------------------------------------------------------------
def test_resolve_fn_call():
    ops = resolve_string("fn greet { } greet")
    fn_call_ops = find_ops(ops, OpKind.FN_CALL)
    assert len(fn_call_ops) == 1
    assert fn_call_ops[0].value == "greet"


def test_resolve_push_variable():
    ops = resolve_string("42 = x x")
    push_var_ops = find_ops(ops, OpKind.PUSH_VARIABLE)
    assert len(push_var_ops) == 1
    assert push_var_ops[0].value == "x"


def test_resolve_struct_new():
    ops = resolve_string("struct Foo { val: int } 42 Foo")
    struct_new_ops = find_ops(ops, OpKind.STRUCT_NEW)
    assert len(struct_new_ops) == 1
    assert isinstance(struct_new_ops[0].value, Struct)


def test_resolve_push_capture():
    code = "42 = x { x }"
    ops = resolve_string(code)
    # The lambda body should have a PUSH_CAPTURE
    fn_push = find_ops(ops, OpKind.FN_PUSH)
    assert len(fn_push) == 1
    lambda_name = fn_push[0].value
    lambda_fn = GLOBAL_FUNCTIONS[lambda_name]
    capture_ops = find_ops(lambda_fn.ops, OpKind.PUSH_CAPTURE)
    assert len(capture_ops) == 1
    assert capture_ops[0].value == "x"


def test_resolve_undefined_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        resolve_string("undefined_thing")
    assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME


# ---------------------------------------------------------------------------
# Function references
# ---------------------------------------------------------------------------
def test_resolve_fn_ref():
    ops = resolve_string("fn greet { } &greet")
    fn_push_ops = find_ops(ops, OpKind.FN_PUSH)
    assert len(fn_push_ops) == 1
    assert fn_push_ops[0].value == "greet"


def test_resolve_fn_ref_method():
    code = """
    struct Foo { val: int }
    impl Foo {
        fn double self:Foo -> int { self Foo::val 2 * }
    }
    &Foo::double
    """
    ops = resolve_string(code)
    fn_push_ops = find_ops(ops, OpKind.FN_PUSH)
    assert len(fn_push_ops) == 1
    assert fn_push_ops[0].value == "Foo::double"


def test_resolve_fn_ref_undefined_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        resolve_string("&undefined")
    assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME


def test_resolve_fn_ref_marks_used():
    resolve_string("fn greet { } &greet")
    assert GLOBAL_FUNCTIONS["greet"].is_used is True


def test_resolve_fn_ref_bare_ampersand_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        resolve_string("&")
    assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX


# ---------------------------------------------------------------------------
# Generic type parameters (parsing)
# ---------------------------------------------------------------------------
def test_parse_generic_function_type_vars():
    parse_string("fn id[T] T -> T { }")
    fn = GLOBAL_FUNCTIONS["id"]
    assert fn.signature is not None
    assert fn.signature.type_vars == {"T"}


def test_parse_generic_multiple_type_vars():
    parse_string("fn swap_t[T1 T2] T1 T2 -> T1 T2 { swap }")
    fn = GLOBAL_FUNCTIONS["swap_t"]
    assert fn.signature.type_vars == {"T1", "T2"}


def test_parse_generic_empty_brackets_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        parse_string("fn foo[] int -> int { }")
    assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX
    assert "Empty type parameter list" in exc_info.value.errors[0].message


def test_parse_generic_invalid_token_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        parse_string("fn foo[42] int -> int { }")
    assert exc_info.value.errors[0].kind == ErrorKind.UNEXPECTED_TOKEN
    assert exc_info.value.errors[0].expected == "type variable name"


@pytest.mark.parametrize("builtin", ["int", "bool", "str", "ptr", "array", "any"])
def test_parse_generic_type_var_shadows_builtin_raises(builtin):
    with pytest.raises(CasaErrorCollection) as exc_info:
        parse_string(f"fn foo[{builtin}] {builtin} -> {builtin} {{ }}")
    assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME
    assert f"shadows built-in type `{builtin}`" in exc_info.value.errors[0].message


def test_parse_generic_type_var_shadows_struct_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        parse_string("struct Foo { val: int } fn id[Foo] Foo -> Foo { }")
    assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME
    assert "shadows struct type `Foo`" in exc_info.value.errors[0].message


# ---------------------------------------------------------------------------
# F-strings
# ---------------------------------------------------------------------------
def test_parse_fstring_plain_text():
    """f"hello" with no expressions produces single PUSH_STR (no FSTRING_CONCAT)."""
    ops = parse_string('f"hello"')
    assert len(ops) == 1
    assert ops[0].kind == OpKind.PUSH_STR
    assert ops[0].value == "hello"


def test_parse_fstring_with_expression():
    """f"hello {x} world" produces PUSH_STR + FN_PUSH + FN_EXEC + PUSH_STR + FSTRING_CONCAT."""
    ops = resolve_string('"hello" = x f"hello {x} world"')
    # Filter to just the f-string related ops (after the variable assignment)
    fstring_ops = [
        op
        for op in ops
        if op.kind not in (OpKind.PUSH_STR, OpKind.ASSIGN_VARIABLE) or ops.index(op) > 1
    ]
    # Should have PUSH_STR("hello "), FN_PUSH + FN_EXEC (expression), PUSH_STR(" world"), FSTRING_CONCAT(3)
    push_str_ops = [op for op in ops if op.kind == OpKind.PUSH_STR]
    fn_push_ops = [op for op in ops if op.kind == OpKind.FN_PUSH]
    fn_exec_ops = [op for op in ops if op.kind == OpKind.FN_EXEC]
    concat_ops = find_ops(ops, OpKind.FSTRING_CONCAT)
    # Original "hello" assignment + f-string text parts
    assert len(push_str_ops) >= 3  # "hello" (assigned), "hello ", " world"
    assert len(fn_push_ops) >= 1
    assert len(fn_exec_ops) >= 1
    assert len(concat_ops) == 1
    assert concat_ops[0].value == 3


def test_parse_fstring_expression_only():
    """f"{x}" with single expression produces FN_PUSH + FN_EXEC (no concat for 1 part)."""
    ops = resolve_string('"hello" = x f"{x}"')
    concat_ops = find_ops(ops, OpKind.FSTRING_CONCAT)
    fn_push_ops = find_ops(ops, OpKind.FN_PUSH)
    # Single expression, no text parts: just lambda push+exec, no concat
    assert len(fn_push_ops) >= 1
    assert len(concat_ops) == 0


def test_parse_fstring_expression_creates_lambda():
    """f-string expressions create lambda functions registered in GLOBAL_FUNCTIONS."""
    ops = resolve_string('"hello" = x f"val: {x}"')
    fn_push_ops = find_ops(ops, OpKind.FN_PUSH)
    assert len(fn_push_ops) >= 1
    lambda_name = fn_push_ops[-1].value
    assert lambda_name in GLOBAL_FUNCTIONS


def test_parse_fstring_inside_function():
    """f-strings work inside function bodies."""
    code = 'fn greet name:str -> str { f"hello {name}" }'
    parse_string(code)
    fn = GLOBAL_FUNCTIONS["greet"]
    push_str_ops = find_ops(fn.ops, OpKind.PUSH_STR)
    assert any(op.value == "hello " for op in push_str_ops)


def test_parse_fstring_escaped_braces():
    r"""f"\{x\}" produces PUSH_STR with literal {x} (no expression)."""
    ops = parse_string(r'f"\{x\}"')
    assert len(ops) == 1
    assert ops[0].kind == OpKind.PUSH_STR
    assert ops[0].value == "{x}"


def test_parse_fstring_empty():
    """f"" produces single PUSH_STR with empty string."""
    ops = parse_string('f""')
    assert len(ops) == 1
    assert ops[0].kind == OpKind.PUSH_STR
    assert ops[0].value == ""


# ---------------------------------------------------------------------------
# fn[sig] as parameter type (parsing)
# ---------------------------------------------------------------------------
def test_parse_fn_type_named_parameter():
    """fn[int -> int] parses correctly as a named parameter type."""
    parse_string("fn apply f:fn[int -> int] x:int -> int { x f exec }")
    fn = GLOBAL_FUNCTIONS["apply"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == ["fn[int -> int]", "int"]
    assert [p.name for p in fn.signature.parameters] == ["f", "x"]
    assert fn.signature.return_types == ["int"]


def test_parse_fn_type_generic_parameter():
    """fn[T -> T] parses as a generic parameter type within a generic function."""
    parse_string("fn apply[T] f:fn[T -> T] x:T -> T { x f exec }")
    fn = GLOBAL_FUNCTIONS["apply"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == ["fn[T -> T]", "T"]
    assert fn.signature.return_types == ["T"]
    assert fn.signature.type_vars == {"T"}


def test_parse_fn_type_nested_brackets():
    """fn[array[int] -> int] with nested brackets parses correctly."""
    parse_string(
        "fn apply f:fn[array[int] -> int] arr:array[int] -> int { arr f exec }"
    )
    fn = GLOBAL_FUNCTIONS["apply"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == [
        "fn[array[int] -> int]",
        "array[int]",
    ]
    assert fn.signature.return_types == ["int"]


def test_parse_fn_type_multi_param():
    """fn[int int -> int] with multiple parameters parses correctly."""
    parse_string("fn apply f:fn[int int -> int] a:int b:int -> int { b a f exec }")
    fn = GLOBAL_FUNCTIONS["apply"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == [
        "fn[int int -> int]",
        "int",
        "int",
    ]


def test_parse_fn_type_unnamed_parameter():
    """fn[int -> int] parses correctly as an unnamed parameter type."""
    parse_string("fn apply fn[int -> int] int -> int { exec }")
    fn = GLOBAL_FUNCTIONS["apply"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == ["fn[int -> int]", "int"]
    assert fn.signature.return_types == ["int"]


def test_parse_fn_type_in_return_type():
    """fn[int -> int] can appear as a return type."""
    parse_string("fn get_fn -> fn[int -> int] { { 1 + } }")
    fn = GLOBAL_FUNCTIONS["get_fn"]
    assert fn.signature is not None
    assert fn.signature.return_types == ["fn[int -> int]"]


def test_parse_fn_type_two_generic_vars():
    """fn[T1 -> T2] parses with two distinct generic type variables."""
    parse_string("fn transform[T1 T2] f:fn[T1 -> T2] x:T1 -> T2 { x f exec }")
    fn = GLOBAL_FUNCTIONS["transform"]
    assert fn.signature is not None
    assert [p.typ for p in fn.signature.parameters] == ["fn[T1 -> T2]", "T1"]
    assert fn.signature.return_types == ["T2"]
    assert fn.signature.type_vars == {"T1", "T2"}
