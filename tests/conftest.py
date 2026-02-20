"""Shared fixtures and pipeline helpers for Casa compiler tests."""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import pytest

from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_STRUCTS,
    GLOBAL_VARIABLES,
    INCLUDED_FILES,
    Cursor,
    Op,
    Program,
    Signature,
    Token,
)
from casa.bytecode import _op_label_map, compile_bytecode, reset_labels
from casa.emitter import emit_program
from casa.error import SOURCE_CACHE
from casa.lexer import Lexer
from casa.parser import parse_ops, resolve_identifiers
from casa.typechecker import type_check_ops

TEST_FILE = Path("test.casa")


def _clear_globals():
    """Reset all module-level mutable state to a clean slate."""
    GLOBAL_FUNCTIONS.clear()
    GLOBAL_STRUCTS.clear()
    GLOBAL_VARIABLES.clear()
    INCLUDED_FILES.clear()
    SOURCE_CACHE.clear()
    reset_labels()
    _op_label_map.clear()


@pytest.fixture(autouse=True)
def clear_global_state():
    """
    Clear all global mutable state before and after every test.

    The pre-yield clear ensures a clean slate even if a previous test's
    teardown was skipped (e.g. due to a crash). The post-yield clear is
    the normal teardown so the current test doesn't leak state.
    """
    _clear_globals()
    yield
    _clear_globals()


def lex_string(code: str) -> list[Token]:
    """Lex a source string into tokens."""
    SOURCE_CACHE[TEST_FILE] = code
    lexer = Lexer(cursor=Cursor(sequence=code), file=TEST_FILE)
    return lexer.lex()


def parse_string(code: str) -> list[Op]:
    """Lex and parse a source string into ops."""
    tokens = lex_string(code)
    return parse_ops(tokens)


def resolve_string(code: str) -> list[Op]:
    """Lex, parse, and resolve identifiers in a source string."""
    ops = parse_string(code)
    return resolve_identifiers(ops)


def typecheck_string(code: str) -> Signature:
    """Full pipeline through type checking. Returns inferred signature."""
    ops = resolve_string(code)
    return type_check_ops(ops)


def compile_string(code: str) -> Program:
    """Full pipeline through bytecode compilation."""
    ops = resolve_string(code)
    type_check_ops(ops)
    return compile_bytecode(ops)


def emit_string(code: str) -> str:
    """Full pipeline to assembly string."""
    program = compile_string(code)
    return emit_program(program)
