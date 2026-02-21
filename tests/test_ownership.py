"""Tests for scope-based ownership system (Phase 1).

Verifies that the type checker enforces ownership rules for owned types
(arrays, structs) vs copy types (int, bool, str, ptr), and that bytecode
emits correct free/clone instructions.
"""

import pytest

from casa.common import GLOBAL_FUNCTIONS, OpKind
from casa.error import CasaErrorCollection, ErrorKind
from casa.typechecker import type_check_ops
from tests.conftest import compile_string, resolve_string, typecheck_string


# ---------------------------------------------------------------------------
# Type checker: dup on owned types
# ---------------------------------------------------------------------------
def test_dup_owned_array_error():
    """dup on owned array type produces DUP_OWNED error."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string("[1, 2, 3] dup")
    assert any(e.kind == ErrorKind.DUP_OWNED for e in exc_info.value.errors)


def test_dup_owned_struct_error():
    """dup on owned struct type produces DUP_OWNED error."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string("struct Foo { x: int }\n42 Foo dup")
    assert any(e.kind == ErrorKind.DUP_OWNED for e in exc_info.value.errors)


# ---------------------------------------------------------------------------
# Type checker: dup on copy types
# ---------------------------------------------------------------------------
def test_dup_copy_types_allowed():
    """dup on copy types (int, bool, str) is allowed."""
    typecheck_string("42 dup drop drop")
    typecheck_string("true dup drop drop")
    typecheck_string('"hello" dup drop drop')


# ---------------------------------------------------------------------------
# Type checker: over on owned types
# ---------------------------------------------------------------------------
def test_over_owned_array_error():
    """over on owned type produces DUP_OWNED error."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string("42 [1, 2] over")
    assert any(e.kind == ErrorKind.DUP_OWNED for e in exc_info.value.errors)


# ---------------------------------------------------------------------------
# Type checker: swap and rot on owned types (moves, not copies)
# ---------------------------------------------------------------------------
def test_swap_owned_types_allowed():
    """swap on owned types is allowed (move, not copy)."""
    typecheck_string("[1, 2] [3, 4] swap drop drop")


def test_rot_owned_types_allowed():
    """rot on owned types is allowed (move, not copy)."""
    typecheck_string("[1] [2] [3] rot drop drop drop")


# ---------------------------------------------------------------------------
# Type checker: clone intrinsic
# ---------------------------------------------------------------------------
def test_clone_owned_array():
    """clone on owned array produces two array values on stack."""
    sig = typecheck_string("[1, 2, 3] clone")
    assert sig.return_types == ["array[int]", "array[int]"]


def test_clone_copy_type_error():
    """clone on copy type is an error (use dup instead)."""
    with pytest.raises(CasaErrorCollection):
        typecheck_string("42 clone")


# ---------------------------------------------------------------------------
# Type checker: free intrinsic
# ---------------------------------------------------------------------------
def test_free_intrinsic():
    """free consumes an owned value from the stack."""
    typecheck_string("[1, 2, 3] free")


# ---------------------------------------------------------------------------
# Type checker: dup inside auto-generated struct getters
# ---------------------------------------------------------------------------
def test_dup_in_auto_generated_getter():
    """Struct getters use dup internally; should not produce DUP_OWNED error."""
    typecheck_string(
        "struct Foo { x: int }\n42 Foo = f\nf.x drop\nf drop"
    )


# ---------------------------------------------------------------------------
# Type checker: capture of owned variable in closure
# ---------------------------------------------------------------------------
def test_capture_owned_in_closure_error():
    """Capturing an owned variable in a closure produces CAPTURE_OWNED error."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        typecheck_string(
            """
fn test_fn {
    [1, 2, 3] = arr
    { arr } exec drop
}
"""
        )
    assert any(e.kind == ErrorKind.CAPTURE_OWNED for e in exc_info.value.errors)


def test_capture_copy_in_closure_allowed():
    """Capturing a copy variable in a closure is allowed."""
    typecheck_string(
        """
fn test_fn {
    42 = x
    { x } exec drop
}
"""
    )


# ---------------------------------------------------------------------------
# Type checker: drop annotation
# ---------------------------------------------------------------------------
def test_drop_owned_annotates_type():
    """drop on owned type annotates op.typ for codegen."""
    ops = resolve_string("[1, 2, 3] drop")
    type_check_ops(ops)
    drop_ops = [o for o in ops if o.kind == OpKind.DROP]
    assert len(drop_ops) == 1
    assert drop_ops[0].typ is not None
    assert "array" in drop_ops[0].typ


def test_drop_copy_no_annotation():
    """drop on copy type does NOT annotate op.typ."""
    ops = resolve_string("42 drop")
    type_check_ops(ops)
    drop_ops = [o for o in ops if o.kind == OpKind.DROP]
    assert len(drop_ops) == 1
    assert drop_ops[0].typ is None


# ---------------------------------------------------------------------------
# Bytecode: drop emits correct free instructions
# ---------------------------------------------------------------------------
def test_drop_owned_array_emits_heap_free_array():
    """drop on owned array emits HEAP_FREE_ARRAY instruction."""
    program = compile_string("[1, 2, 3] drop")
    inst_kinds = [i.kind.name for i in program.bytecode]
    assert "HEAP_FREE_ARRAY" in inst_kinds


def test_drop_owned_struct_emits_heap_free():
    """drop on owned struct emits HEAP_FREE instruction."""
    program = compile_string("struct Foo { x: int }\n42 Foo drop")
    inst_kinds = [i.kind.name for i in program.bytecode]
    assert "HEAP_FREE" in inst_kinds


# ---------------------------------------------------------------------------
# Bytecode: clone emits correct clone instructions
# ---------------------------------------------------------------------------
def test_clone_array_emits_clone_array():
    """clone on array emits CLONE_ARRAY instruction."""
    program = compile_string("[1, 2, 3] clone drop drop")
    inst_kinds = [i.kind.name for i in program.bytecode]
    assert "CLONE_ARRAY" in inst_kinds


def test_clone_struct_emits_clone_struct():
    """clone on struct emits CLONE_STRUCT instruction."""
    program = compile_string("struct Foo { x: int }\n42 Foo clone drop drop")
    inst_kinds = [i.kind.name for i in program.bytecode]
    assert "CLONE_STRUCT" in inst_kinds
