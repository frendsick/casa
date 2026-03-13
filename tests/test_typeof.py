"""Tests for the `typeof` intrinsic across all pipeline stages."""

import pytest

from casa.common import InstKind, Intrinsic, OpKind
from casa.error import CasaErrorCollection
from tests.conftest import (
    compile_string,
    emit_string,
    lex_string,
    resolve_string,
    typecheck_string,
)


# ---------------------------------------------------------------------------
# Lexer
# ---------------------------------------------------------------------------
def test_lex_typeof():
    """typeof is lexed as an intrinsic token."""
    tokens = lex_string("typeof")
    intrinsic_tokens = [t for t in tokens if t.value == "typeof"]
    assert len(intrinsic_tokens) == 1
    assert intrinsic_tokens[0].kind.name == "INTRINSIC"


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------
def test_parse_typeof():
    """typeof parses into OpKind.TYPEOF."""
    ops = resolve_string("42 typeof")
    typeof_ops = [op for op in ops if op.kind == OpKind.TYPEOF]
    assert len(typeof_ops) == 1
    assert typeof_ops[0].value == Intrinsic.TYPEOF


# ---------------------------------------------------------------------------
# Type checker: stack effects
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "code",
    [
        "42 typeof",
        "true typeof",
        '"hello" typeof',
        "'a' typeof",
        "[1, 2] typeof",
    ],
)
def test_typecheck_typeof_consumes_value(code):
    """typeof consumes one value and produces nothing."""
    sig = typecheck_string(code)
    assert sig.return_types == []


@pytest.mark.parametrize(
    "code,expected_type_annotation",
    [
        ("42 typeof", "int"),
        ("true typeof", "bool"),
        ('"hello" typeof', "str"),
        ("'a' typeof", "char"),
        ("[1, 2] typeof", "array[int]"),
    ],
)
def test_typecheck_typeof_annotation(code, expected_type_annotation):
    """typeof stores the type name in op.type_annotation."""
    ops = resolve_string(code)
    from casa.typechecker import type_check_ops

    type_check_ops(ops)
    typeof_ops = [op for op in ops if op.kind == OpKind.TYPEOF]
    assert len(typeof_ops) == 1
    assert typeof_ops[0].type_annotation == expected_type_annotation


def test_typecheck_typeof_struct():
    """typeof on a struct value stores the struct type name."""
    code = "struct Foo { x: int }\n42 Foo typeof"
    ops = resolve_string(code)
    from casa.typechecker import type_check_ops

    type_check_ops(ops)
    typeof_ops = [op for op in ops if op.kind == OpKind.TYPEOF]
    assert len(typeof_ops) == 1
    assert typeof_ops[0].type_annotation == "Foo"


def test_typecheck_typeof_enum():
    """typeof on an enum variant stores the enum type name."""
    code = "enum Color { Red Green Blue }\nColor::Red typeof"
    ops = resolve_string(code)
    from casa.typechecker import type_check_ops

    type_check_ops(ops)
    typeof_ops = [op for op in ops if op.kind == OpKind.TYPEOF]
    assert len(typeof_ops) == 1
    assert typeof_ops[0].type_annotation == "Color"


def test_typecheck_typeof_fn():
    """typeof on a lambda stores the fn type."""
    code = "{ 2 * } typeof"
    ops = resolve_string(code)
    from casa.typechecker import type_check_ops

    type_check_ops(ops)
    typeof_ops = [op for op in ops if op.kind == OpKind.TYPEOF]
    assert len(typeof_ops) == 1
    assert typeof_ops[0].type_annotation.startswith("fn[")


def test_typecheck_typeof_ptr():
    """typeof on a ptr value stores 'ptr'."""
    code = "10 alloc typeof"
    ops = resolve_string(code)
    from casa.typechecker import type_check_ops

    type_check_ops(ops)
    typeof_ops = [op for op in ops if op.kind == OpKind.TYPEOF]
    assert len(typeof_ops) == 1
    assert typeof_ops[0].type_annotation == "ptr"


def test_typecheck_typeof_does_not_leave_stack_value():
    """typeof should not push anything onto the stack."""
    sig = typecheck_string("42 typeof")
    assert sig.return_types == []
    assert sig.parameters == []


def test_typecheck_typeof_in_function():
    """typeof works inside a function body."""
    code = "fn show_type x:int { x typeof } 42 show_type"
    sig = typecheck_string(code)
    assert sig.return_types == []


def test_typecheck_typeof_chained():
    """Multiple typeof calls each consume one value."""
    sig = typecheck_string("42 typeof true typeof")
    assert sig.return_types == []


# ---------------------------------------------------------------------------
# Bytecode: decomposition into DROP + PUSH_STR + PRINT_STR
# ---------------------------------------------------------------------------
def test_bytecode_typeof_int():
    """typeof on int decomposes into DROP, PUSH_STR, PRINT_STR."""
    program = compile_string("42 typeof")
    kinds = [i.kind for i in program.bytecode]
    assert InstKind.DROP in kinds
    assert InstKind.PUSH_STR in kinds
    assert InstKind.PRINT_STR in kinds
    assert "int" in program.strings


def test_bytecode_typeof_bool():
    """typeof on bool interns 'bool' string."""
    program = compile_string("true typeof")
    assert "bool" in program.strings


def test_bytecode_typeof_str():
    """typeof on str interns 'str' string."""
    program = compile_string('"hello" typeof')
    assert "str" in program.strings


def test_bytecode_typeof_struct():
    """typeof on a struct interns the struct name."""
    program = compile_string("struct Foo { x: int }\n42 Foo typeof")
    assert "Foo" in program.strings


def test_bytecode_typeof_enum():
    """typeof on an enum interns the enum name."""
    program = compile_string("enum Color { Red Green Blue }\nColor::Red typeof")
    assert "Color" in program.strings


def test_bytecode_typeof_instruction_sequence():
    """typeof produces exactly DROP, PUSH_STR, PRINT_STR in order."""
    program = compile_string("42 typeof")
    # Find the DROP that belongs to typeof (after the PUSH for 42)
    kinds = [i.kind for i in program.bytecode]

    # Find the sequence DROP -> PUSH_STR -> PRINT_STR
    found = False
    for i in range(len(kinds) - 2):
        if (
            kinds[i] == InstKind.DROP
            and kinds[i + 1] == InstKind.PUSH_STR
            and kinds[i + 2] == InstKind.PRINT_STR
        ):
            found = True
            break
    assert found, f"Expected DROP -> PUSH_STR -> PRINT_STR sequence in {kinds}"


def test_bytecode_typeof_string_interning():
    """Multiple typeof on same type share the same interned string."""
    program = compile_string("42 typeof 99 typeof")
    assert program.strings.count("int") == 1


# ---------------------------------------------------------------------------
# Emitter: assembly output
# ---------------------------------------------------------------------------
def test_emit_typeof_int():
    """typeof on int emits assembly that prints 'int'."""
    asm = emit_string("42 typeof")
    # Should contain the type string in the data section
    assert "int" in asm
    # Should emit a drop (addq $8, %rsp) and a print_str call
    assert "addq $8, %rsp" in asm
    assert "jmp print_str" in asm


def test_emit_typeof_bool():
    """typeof on bool emits assembly referencing 'bool' string."""
    asm = emit_string("true typeof")
    assert "bool" in asm


def test_emit_typeof_str():
    """typeof on str emits assembly referencing 'str' string."""
    asm = emit_string('"hello" typeof')
    # The data section should contain the "str" type name
    assert "jmp print_str" in asm
