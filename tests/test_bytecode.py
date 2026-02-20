"""Tests for casa/bytecode.py — verifies Program structure, instruction kinds, string table."""

import pytest

from casa.common import InstKind, Program
from tests.conftest import compile_string


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def find_insts(program: Program, kind: InstKind, in_functions: bool = False):
    """Find instructions of a given kind in main bytecode or all function bytecodes."""
    results = [i for i in program.bytecode if i.kind == kind]
    if in_functions:
        for fn_bc in program.functions.values():
            results += [i for i in fn_bc if i.kind == kind]
    return results


# ---------------------------------------------------------------------------
# Simple ops (1:1 mapping from OpKind to InstKind)
# ---------------------------------------------------------------------------
SIMPLE_OP_CASES = [
    ("1 2 +", InstKind.ADD),
    ("1 2 -", InstKind.SUB),
    ("1 2 *", InstKind.MUL),
    ("1 2 /", InstKind.DIV),
    ("1 2 %", InstKind.MOD),
    ("1 2 <<", InstKind.SHL),
    ("1 2 >>", InstKind.SHR),
    ("true false &&", InstKind.AND),
    ("true false ||", InstKind.OR),
    ("true !", InstKind.NOT),
    ("1 2 ==", InstKind.EQ),
    ("1 2 !=", InstKind.NE),
    ("1 2 >", InstKind.GT),
    ("1 2 >=", InstKind.GE),
    ("1 2 <", InstKind.LT),
    ("1 2 <=", InstKind.LE),
    ("1 drop", InstKind.DROP),
    ("1 dup", InstKind.DUP),
    ("1 2 swap", InstKind.SWAP),
    ("1 2 over", InstKind.OVER),
    ("1 2 3 rot", InstKind.ROT),
]


@pytest.mark.parametrize("code,expected_kind", SIMPLE_OP_CASES)
def test_bytecode_simple_ops(code, expected_kind):
    program = compile_string(code)
    insts = find_insts(program, expected_kind)
    assert len(insts) >= 1


# ---------------------------------------------------------------------------
# Push instructions
# ---------------------------------------------------------------------------
def test_bytecode_push_int():
    program = compile_string("42")
    pushes = find_insts(program, InstKind.PUSH)
    assert any(i.args == [42] for i in pushes)


def test_bytecode_push_bool_true():
    program = compile_string("true")
    pushes = find_insts(program, InstKind.PUSH)
    assert any(i.args == [1] for i in pushes)


def test_bytecode_push_bool_false():
    program = compile_string("false")
    pushes = find_insts(program, InstKind.PUSH)
    assert any(i.args == [0] for i in pushes)


def test_bytecode_push_str():
    program = compile_string('"hello"')
    pushes = find_insts(program, InstKind.PUSH_STR)
    assert len(pushes) >= 1
    assert "hello" in program.strings


# ---------------------------------------------------------------------------
# Push array
# ---------------------------------------------------------------------------
def test_bytecode_push_array():
    program = compile_string("[1, 2, 3]")
    kinds = [i.kind for i in program.bytecode]

    # Array items pushed in reverse order
    pushes = find_insts(program, InstKind.PUSH)
    push_values = [i.args[0] for i in pushes]
    assert push_values[:3] == [3, 2, 1]

    # Heap allocation for array (1 length slot + 3 items = 4 slots)
    assert InstKind.HEAP_ALLOC in kinds

    # Store loop: labels, jumps, and stores
    assert InstKind.LABEL in kinds
    assert InstKind.JUMP in kinds
    assert InstKind.JUMP_NE in kinds
    assert InstKind.STORE in kinds

    # 2 locals: array pointer and loop index (no local for length)
    local_sets = find_insts(program, InstKind.LOCAL_SET)
    local_gets = find_insts(program, InstKind.LOCAL_GET)
    assert len(local_sets) >= 2
    assert len(local_gets) >= 2

    # Length is pushed as a constant, not loaded from a local
    # The PUSH(3) for the length comparison should appear in the loop
    assert any(i.args == [3] for i in pushes)


# ---------------------------------------------------------------------------
# Print
# ---------------------------------------------------------------------------
def test_bytecode_print_int():
    program = compile_string("42 print")
    assert len(find_insts(program, InstKind.PRINT_INT)) >= 1


def test_bytecode_print_str():
    program = compile_string('"hello" print')
    assert len(find_insts(program, InstKind.PRINT_STR)) >= 1


# ---------------------------------------------------------------------------
# Heap
# ---------------------------------------------------------------------------
def test_bytecode_heap_alloc():
    program = compile_string("10 alloc")
    assert len(find_insts(program, InstKind.HEAP_ALLOC)) >= 1


def test_bytecode_load_store():
    program = compile_string("42 10 alloc store 10 alloc load")
    assert len(find_insts(program, InstKind.STORE)) >= 1
    assert len(find_insts(program, InstKind.LOAD)) >= 1


# ---------------------------------------------------------------------------
# Globals
# ---------------------------------------------------------------------------
def test_bytecode_globals():
    program = compile_string("42 = x x")
    assert program.globals_count >= 1
    assert len(find_insts(program, InstKind.GLOBALS_INIT)) >= 1
    assert len(find_insts(program, InstKind.GLOBAL_SET)) >= 1
    assert len(find_insts(program, InstKind.GLOBAL_GET)) >= 1


# ---------------------------------------------------------------------------
# Locals
# ---------------------------------------------------------------------------
def test_bytecode_locals():
    code = "fn foo a:int -> int { a }\n5 foo"
    program = compile_string(code)
    all_insts = find_insts(program, InstKind.LOCALS_INIT, in_functions=True)
    assert len(all_insts) >= 1
    all_insts = find_insts(program, InstKind.LOCALS_UNINIT, in_functions=True)
    assert len(all_insts) >= 1
    all_insts = find_insts(program, InstKind.LOCAL_SET, in_functions=True)
    assert len(all_insts) >= 1
    all_insts = find_insts(program, InstKind.LOCAL_GET, in_functions=True)
    assert len(all_insts) >= 1


# ---------------------------------------------------------------------------
# Function call / return
# ---------------------------------------------------------------------------
def test_bytecode_fn_call():
    code = "fn greet { } greet"
    program = compile_string(code)
    calls = find_insts(program, InstKind.FN_CALL)
    assert len(calls) >= 1
    assert calls[0].args == ["greet"]
    assert "greet" in program.functions


def test_bytecode_fn_return():
    code = "fn noop { } noop"
    program = compile_string(code)
    returns = find_insts(program, InstKind.FN_RETURN, in_functions=True)
    assert len(returns) >= 1


# ---------------------------------------------------------------------------
# Lambda: FN_PUSH / FN_EXEC
# ---------------------------------------------------------------------------
def test_bytecode_fn_push_exec():
    code = "42 { 1 + } exec"
    program = compile_string(code)
    pushes = find_insts(program, InstKind.FN_PUSH)
    assert len(pushes) >= 1
    execs = find_insts(program, InstKind.FN_EXEC)
    assert len(execs) >= 1


# ---------------------------------------------------------------------------
# Constants (captures)
# ---------------------------------------------------------------------------
def test_bytecode_constants():
    code = "42 = x { x } exec"
    program = compile_string(code)
    assert program.constants_count >= 1
    stores = find_insts(program, InstKind.CONSTANT_STORE)
    assert len(stores) >= 1
    loads = find_insts(program, InstKind.CONSTANT_LOAD, in_functions=True)
    assert len(loads) >= 1


# ---------------------------------------------------------------------------
# Control flow: if
# ---------------------------------------------------------------------------
def test_bytecode_if_control_flow():
    code = "if true then 1 drop fi"
    program = compile_string(code)
    labels = find_insts(program, InstKind.LABEL)
    assert len(labels) >= 1
    jumps_ne = find_insts(program, InstKind.JUMP_NE)
    assert len(jumps_ne) >= 1


# ---------------------------------------------------------------------------
# Control flow: while
# ---------------------------------------------------------------------------
def test_bytecode_while_control_flow():
    code = "while false do done"
    program = compile_string(code)
    labels = find_insts(program, InstKind.LABEL)
    assert len(labels) >= 2  # loop start + loop end
    jumps = find_insts(program, InstKind.JUMP)
    assert len(jumps) >= 1
    jumps_ne = find_insts(program, InstKind.JUMP_NE)
    assert len(jumps_ne) >= 1


# ---------------------------------------------------------------------------
# String interning
# ---------------------------------------------------------------------------
def test_bytecode_string_interning():
    code = '"hello" print "hello" print "world" print'
    program = compile_string(code)
    assert program.strings.count("hello") == 1
    assert "world" in program.strings
    assert len(program.strings) == 2
