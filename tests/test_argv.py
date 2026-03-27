"""Tests for the `argc` and `argv` intrinsics across all pipeline stages."""

from casa.common import InstKind, Intrinsic, OpKind
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
def test_lex_argc():
    """argc is lexed as an intrinsic token."""
    tokens = lex_string("argc")
    intrinsic_tokens = [t for t in tokens if t.value == "argc"]
    assert len(intrinsic_tokens) == 1
    assert intrinsic_tokens[0].kind.name == "INTRINSIC"


def test_lex_argv():
    """argv is lexed as an intrinsic token."""
    tokens = lex_string("argv")
    intrinsic_tokens = [t for t in tokens if t.value == "argv"]
    assert len(intrinsic_tokens) == 1
    assert intrinsic_tokens[0].kind.name == "INTRINSIC"


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------
def test_parse_argc():
    """argc parses into OpKind.ARGC."""
    ops = resolve_string("argc")
    argc_ops = [op for op in ops if op.kind == OpKind.ARGC]
    assert len(argc_ops) == 1
    assert argc_ops[0].value == Intrinsic.ARGC


def test_parse_argv():
    """argv parses into OpKind.ARGV."""
    ops = resolve_string("argv")
    argv_ops = [op for op in ops if op.kind == OpKind.ARGV]
    assert len(argv_ops) == 1
    assert argv_ops[0].value == Intrinsic.ARGV


# ---------------------------------------------------------------------------
# Type checker
# ---------------------------------------------------------------------------
def test_typecheck_argc_pushes_int():
    """argc pushes one int onto the stack."""
    sig = typecheck_string("argc")
    assert sig.return_types == ["int"]


def test_typecheck_argv_pushes_ptr():
    """argv pushes one ptr onto the stack."""
    sig = typecheck_string("argv")
    assert sig.return_types == ["ptr"]


def test_typecheck_argc_no_inputs():
    """argc requires nothing on the stack."""
    sig = typecheck_string("argc")
    assert sig.parameters == []


def test_typecheck_argv_no_inputs():
    """argv requires nothing on the stack."""
    sig = typecheck_string("argv")
    assert sig.parameters == []


def test_typecheck_argc_in_arithmetic():
    """argc result can be used in int arithmetic."""
    sig = typecheck_string("argc 1 +")
    assert sig.return_types == ["int"]


def test_typecheck_argv_with_load():
    """argv result can be used with load64."""
    sig = typecheck_string("argv load64")
    assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# Bytecode
# ---------------------------------------------------------------------------
def test_bytecode_argc():
    """argc compiles to InstKind.ARGC."""
    program = compile_string("argc")
    kinds = [i.kind for i in program.bytecode]
    assert InstKind.ARGC in kinds


def test_bytecode_argv():
    """argv compiles to InstKind.ARGV."""
    program = compile_string("argv")
    kinds = [i.kind for i in program.bytecode]
    assert InstKind.ARGV in kinds


def test_bytecode_argc_no_args():
    """ARGC instruction has no arguments."""
    program = compile_string("argc")
    argc_insts = [i for i in program.bytecode if i.kind == InstKind.ARGC]
    assert len(argc_insts) == 1
    assert argc_insts[0].args == []


def test_bytecode_argv_no_args():
    """ARGV instruction has no arguments."""
    program = compile_string("argv")
    argv_insts = [i for i in program.bytecode if i.kind == InstKind.ARGV]
    assert len(argv_insts) == 1
    assert argv_insts[0].args == []


# ---------------------------------------------------------------------------
# Emitter
# ---------------------------------------------------------------------------
def test_emit_argc_loads_from_bss():
    """argc emits a load from the __argc BSS slot."""
    asm = emit_string("argc")
    assert "__argc(%rip)" in asm


def test_emit_argv_loads_from_bss():
    """argv emits a load from the __argv BSS slot."""
    asm = emit_string("argv")
    assert "__argv(%rip)" in asm


def test_emit_bss_has_argc_argv_slots():
    """BSS section declares __argc and __argv storage."""
    asm = emit_string("42")
    assert "__argc: .skip 8" in asm
    assert "__argv: .skip 8" in asm


def test_emit_start_saves_argc_argv():
    """_start saves argc and argv before user code."""
    asm = emit_string("42")
    start_idx = asm.index("_start:")
    # The saves must appear between _start and any user code
    after_start = asm[start_idx:]
    assert "movq (%rsp), %rax" in after_start
    assert "__argc(%rip)" in after_start
    assert "leaq 8(%rsp)" in after_start
    assert "__argv(%rip)" in after_start
