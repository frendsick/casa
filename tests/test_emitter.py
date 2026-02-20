"""Tests for casa/emitter.py — verifies assembly output contains expected patterns."""

import pytest

from tests.conftest import emit_string


# ---------------------------------------------------------------------------
# Sections
# ---------------------------------------------------------------------------
def test_emit_sections():
    asm = emit_string("42")
    assert ".section .bss" in asm
    assert ".section .data" in asm
    assert ".section .text" in asm
    assert ".globl _start" in asm


# ---------------------------------------------------------------------------
# Push
# ---------------------------------------------------------------------------
def test_emit_push_int():
    asm = emit_string("42")
    assert "pushq $42" in asm


def test_emit_negative_int():
    asm = emit_string("-42")
    assert "pushq $-42" in asm


def test_emit_large_negative_int():
    large_neg = -(2**32)
    asm = emit_string(str(large_neg))
    assert "movabsq" in asm


def test_emit_push_large_int():
    large = 2**32
    asm = emit_string(str(large))
    assert "movabsq" in asm


def test_emit_push_str():
    asm = emit_string('"hello"')
    assert 'str_0: .asciz "hello"' in asm


def test_emit_escaped_newline_str():
    asm = emit_string(r'"hello\nworld"')
    assert 'str_0: .asciz "hello\\nworld"' in asm


def test_emit_escaped_tab_str():
    asm = emit_string(r'"col1\tcol2"')
    assert 'str_0: .asciz "col1\\tcol2"' in asm


def test_emit_escaped_backslash_str():
    asm = emit_string(r'"path\\file"')
    assert 'str_0: .asciz "path\\\\file"' in asm


def test_emit_escaped_carriage_return_str():
    asm = emit_string(r'"line\r"')
    assert 'str_0: .asciz "line\\r"' in asm


def test_emit_escaped_quote_str():
    asm = emit_string(r'"say \"hi\""')
    assert r'str_0: .asciz "say \"hi\""' in asm


def test_emit_escaped_null_str():
    asm = emit_string(r'"hello\0world"')
    assert 'str_0: .asciz "hello\\0world"' in asm


# ---------------------------------------------------------------------------
# Stack operations
# ---------------------------------------------------------------------------
def test_emit_drop():
    asm = emit_string("42 drop")
    assert "addq $8, %rsp" in asm


def test_emit_dup():
    asm = emit_string("42 dup")
    assert "pushq (%rsp)" in asm


def test_emit_swap():
    asm = emit_string("1 2 swap")
    assert "popq %rax" in asm
    assert "popq %rbx" in asm
    assert "pushq %rax" in asm
    assert "pushq %rbx" in asm


def test_emit_over():
    asm = emit_string("1 2 over")
    assert "pushq 8(%rsp)" in asm


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "code,expected_asm",
    [
        ("1 2 +", "addq"),
        ("1 2 -", "subq"),
        ("1 2 *", "imulq"),
        ("1 2 /", "idivq"),
    ],
)
def test_emit_arithmetic(code, expected_asm):
    asm = emit_string(code)
    assert expected_asm in asm


# ---------------------------------------------------------------------------
# Bitshift
# ---------------------------------------------------------------------------
def test_emit_shl():
    asm = emit_string("8 2 <<")
    assert "shlq %cl" in asm


def test_emit_shr():
    asm = emit_string("8 2 >>")
    assert "sarq %cl" in asm


# ---------------------------------------------------------------------------
# Boolean
# ---------------------------------------------------------------------------
def test_emit_and():
    asm = emit_string("true false &&")
    assert "andb" in asm


def test_emit_or():
    asm = emit_string("true false ||")
    assert "orq" in asm


def test_emit_not():
    asm = emit_string("true !")
    assert "sete" in asm


# ---------------------------------------------------------------------------
# Comparison
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "code,expected_setcc",
    [
        ("1 2 ==", "sete"),
        ("1 2 !=", "setne"),
        ("1 2 <", "setl"),
        ("1 2 <=", "setle"),
        ("1 2 >", "setg"),
        ("1 2 >=", "setge"),
    ],
)
def test_emit_comparison(code, expected_setcc):
    asm = emit_string(code)
    assert expected_setcc in asm


# ---------------------------------------------------------------------------
# Labels and jumps
# ---------------------------------------------------------------------------
def test_emit_label_jump():
    asm = emit_string("if true then 1 drop fi")
    assert ".L" in asm
    assert "jz" in asm


def test_emit_while_jump():
    asm = emit_string("while false do done")
    assert "jmp .L" in asm
    assert "jz .L" in asm


# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------
def test_emit_fn_call():
    asm = emit_string("fn greet { } greet")
    assert "jmp fn_greet" in asm
    assert ".Lret_" in asm


def test_emit_fn_return():
    asm = emit_string("fn noop { } noop")
    assert "jmpq *(%r14)" in asm


def test_emit_fn_exec():
    asm = emit_string("42 { 1 + } exec")
    assert "jmpq *%rax" in asm


def test_emit_fn_push():
    asm = emit_string("{ 1 + }")
    assert "leaq fn_lambda__" in asm


# ---------------------------------------------------------------------------
# Globals
# ---------------------------------------------------------------------------
def test_emit_global_get_set():
    asm = emit_string("42 = x x")
    assert "globals+" in asm


# ---------------------------------------------------------------------------
# Locals
# ---------------------------------------------------------------------------
def test_emit_local_get_set():
    asm = emit_string("fn foo a:int -> int { a }\n5 foo")
    assert "(%r14)" in asm


def test_emit_locals_init_uninit():
    asm = emit_string("fn foo a:int -> int { a }\n5 foo")
    assert "rep stosq" in asm


# ---------------------------------------------------------------------------
# Constants (captures)
# ---------------------------------------------------------------------------
def test_emit_constant_load_store():
    asm = emit_string("42 = x { x } exec")
    assert "constants+" in asm


# ---------------------------------------------------------------------------
# Heap
# ---------------------------------------------------------------------------
def test_emit_heap_alloc():
    asm = emit_string("10 alloc")
    assert "heap_ptr" in asm


def test_emit_load_store():
    asm = emit_string("42 10 alloc store")
    assert "leaq heap(%rip)" in asm


# ---------------------------------------------------------------------------
# Print
# ---------------------------------------------------------------------------
def test_emit_print_int():
    asm = emit_string("42 print")
    assert "jmp print_int" in asm


def test_emit_print_int_no_newline():
    """print_int helper should not append a newline to the output."""
    asm = emit_string("42 print")
    lines = asm.split("\n")
    in_print_int = False
    for line in lines:
        if "print_int:" in line:
            in_print_int = True
        elif in_print_int and line.strip() and not line.startswith((" ", "\t")):
            break
        elif in_print_int:
            assert "movb $10" not in line, "print_int should not append a newline"


def test_emit_print_str():
    asm = emit_string('"hello" print')
    assert "jmp print_str" in asm


def test_emit_print_str_no_newline():
    """print_str helper should not write a newline after the string."""
    asm = emit_string('"hello" print')
    lines = asm.split("\n")
    in_print_str = False
    for line in lines:
        if "print_str:" in line:
            in_print_str = True
        elif in_print_str and line.strip() and not line.startswith((" ", "\t")):
            break
        elif in_print_str:
            assert "newline" not in line, "print_str should not reference newline"


# ---------------------------------------------------------------------------
# Exit syscall
# ---------------------------------------------------------------------------
def test_emit_exit_syscall():
    asm = emit_string("42")
    assert "movq $60, %rax" in asm
    assert "syscall" in asm


# ---------------------------------------------------------------------------
# Name sanitization
# ---------------------------------------------------------------------------
def test_emit_sanitized_names():
    code = """
    struct Foo { x: int }
    42 Foo .x
    """
    asm = emit_string(code)
    assert "fn_Foo__x" in asm
