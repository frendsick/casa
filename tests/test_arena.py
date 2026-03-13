"""End-to-end tests for Arena allocator.

These tests verify that programs using the Arena struct compile through
the full pipeline (lex -> parse -> resolve -> typecheck -> bytecode -> emit).
"""

from tests.conftest import emit_string, typecheck_string

STD_INCLUDE = 'include "lib/std.casa"\n'


# ---------------------------------------------------------------------------
# Construction (Arena::new)
# ---------------------------------------------------------------------------
def test_arena_new_compiles():
    """Arena::new compiles through the full pipeline."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    arena.size print
    """
    emit_string(code)


def test_arena_new_returns_arena_type():
    """Arena::new returns Arena type on the stack."""
    code = STD_INCLUDE + """
    1024 Arena::new
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["Arena"]


# ---------------------------------------------------------------------------
# Allocation (Arena::alloc)
# ---------------------------------------------------------------------------
def test_arena_alloc_compiles():
    """Arena::alloc compiles through the full pipeline."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena Arena::alloc = p
    p print
    """
    emit_string(code)


def test_arena_alloc_returns_ptr_type():
    """Arena::alloc returns ptr type on the stack."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena Arena::alloc
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["ptr"]


def test_arena_alloc_dot_syntax():
    """Arena::alloc works with dot syntax."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc = p
    p print
    """
    emit_string(code)


def test_arena_alloc_multiple():
    """Multiple arena allocations compile correctly."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc = p1
    8 arena.alloc = p2
    2 arena.alloc = p3
    p1 print
    p2 print
    p3 print
    """
    emit_string(code)


# ---------------------------------------------------------------------------
# Store / Load (arena-allocated memory)
# ---------------------------------------------------------------------------
def test_arena_store_load():
    """Arena-allocated memory can be written to and read from."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    1 arena.alloc = p
    42 p store
    p load print
    """
    emit_string(code)


def test_arena_store_load_multiple_slots():
    """Multiple slots in arena-allocated memory work correctly."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc = p
    10 p store
    20 p 1 + store
    30 p 2 + store
    40 p 3 + store
    p load print
    p 1 + load print
    p 2 + load print
    p 3 + load print
    """
    emit_string(code)


def test_arena_separate_allocations_independent():
    """Separate arena allocations hold independent values."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    1 arena.alloc = p1
    1 arena.alloc = p2
    100 p1 store
    200 p2 store
    p1 load print
    p2 load print
    """
    emit_string(code)


# ---------------------------------------------------------------------------
# Reset (Arena::reset)
# ---------------------------------------------------------------------------
def test_arena_reset_compiles():
    """Arena::reset compiles through the full pipeline."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc drop
    arena Arena::reset
    """
    emit_string(code)


def test_arena_reset_dot_syntax():
    """Arena::reset works with dot syntax."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc drop
    arena.reset
    """
    emit_string(code)


def test_arena_reuse_after_reset():
    """Arena can allocate again after reset."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc = p1
    42 p1 store
    arena.reset
    4 arena.alloc = p2
    99 p2 store
    p2 load print
    """
    emit_string(code)


# ---------------------------------------------------------------------------
# used / available
# ---------------------------------------------------------------------------
def test_arena_used_compiles():
    """Arena::used compiles through the full pipeline."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc drop
    arena Arena::used print
    """
    emit_string(code)


def test_arena_used_returns_int_type():
    """Arena::used returns int type on the stack."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc drop
    arena Arena::used
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_arena_used_dot_syntax():
    """Arena::used works with dot syntax."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc drop
    arena.used print
    """
    emit_string(code)


def test_arena_available_compiles():
    """Arena::available compiles through the full pipeline."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc drop
    arena Arena::available print
    """
    emit_string(code)


def test_arena_available_returns_int_type():
    """Arena::available returns int type on the stack."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc drop
    arena Arena::available
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_arena_available_dot_syntax():
    """Arena::available works with dot syntax."""
    code = STD_INCLUDE + """
    1024 Arena::new = arena
    4 arena.alloc drop
    arena.available print
    """
    emit_string(code)


# ---------------------------------------------------------------------------
# Integration (full lifecycle)
# ---------------------------------------------------------------------------
def test_arena_full_lifecycle():
    """Full arena lifecycle: create, alloc, store, load, reset, reuse."""
    code = STD_INCLUDE + """
    256 Arena::new = arena

    # First allocation phase
    2 arena.alloc = p1
    42 p1 store
    99 p1 1 + store
    p1 load print
    p1 1 + load print
    arena.used print

    # Reset and reuse
    arena.reset
    arena.used print

    # Second allocation phase
    1 arena.alloc = p2
    77 p2 store
    p2 load print
    arena.available print
    """
    emit_string(code)


def test_arena_multiple_alloc_and_query():
    """Multiple allocations with used/available queries compile correctly."""
    code = STD_INCLUDE + """
    100 Arena::new = arena
    10 arena.alloc drop
    arena.used print
    arena.available print
    20 arena.alloc drop
    arena.used print
    arena.available print
    """
    emit_string(code)


def test_arena_reset_restores_capacity():
    """After reset, the full arena capacity is available again."""
    code = STD_INCLUDE + """
    64 Arena::new = arena
    32 arena.alloc drop
    arena.reset
    arena.used print
    arena.available print
    """
    emit_string(code)
