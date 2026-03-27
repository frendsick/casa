"""Tests for generic struct type parameters (struct Name[T] { ... })."""

import pytest

from casa.common import GLOBAL_FUNCTIONS, GLOBAL_STRUCTS, Op, OpKind, Struct
from casa.error import CasaErrorCollection, ErrorKind
from tests.conftest import (
    compile_string,
    emit_string,
    parse_string,
    resolve_string,
    typecheck_string,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def find_ops(ops: list[Op], kind: OpKind) -> list[Op]:
    return [op for op in ops if op.kind == kind]


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------
def test_parse_generic_struct():
    parse_string("struct Box[T] { value: T }")
    assert "Box" in GLOBAL_STRUCTS
    struct = GLOBAL_STRUCTS["Box"]
    assert struct.type_vars == ["T"]
    assert struct.members[0].name == "value"
    assert struct.members[0].typ == "T"


def test_parse_generic_struct_multiple_type_vars():
    parse_string("struct Pair[A B] { first: A second: B }")
    struct = GLOBAL_STRUCTS["Pair"]
    assert struct.type_vars == ["A", "B"]


def test_parse_struct_rejects_trait_bounds():
    """Struct definitions must not have trait bounds — those belong on impl blocks."""
    with pytest.raises(CasaErrorCollection):
        parse_string(
            "trait Hashable { fn hash int -> int }\nstruct Set[K: Hashable] { data: K }"
        )


def test_parse_generic_struct_generates_generic_getter():
    parse_string("struct Box[T] { value: T }")
    getter = GLOBAL_FUNCTIONS["Box::value"]
    assert getter.signature.type_vars == {"T"}
    assert getter.signature.parameters[0].typ == "Box[T]"
    assert getter.signature.return_types == ["T"]


def test_parse_generic_struct_generates_generic_setter():
    parse_string("struct Box[T] { value: T }")
    setter = GLOBAL_FUNCTIONS["Box::set_value"]
    assert setter.signature.type_vars == {"T"}
    assert setter.signature.parameters[0].typ == "Box[T]"
    assert setter.signature.parameters[1].typ == "T"


def test_parse_impl_with_type_params():
    """impl[K: Bound] Type[K] propagates type vars and bounds to methods."""
    code = (
        "trait Hashable { fn hash int -> int }\n"
        "struct Box[K] { data: K }\n"
        "impl[K: Hashable] Box[K] {\n"
        "    fn get self:Box[K] -> K { self .data }\n"
        "}\n"
    )
    parse_string(code)
    getter = GLOBAL_FUNCTIONS["Box::get"]
    assert "K" in getter.signature.type_vars
    assert getter.signature.trait_bounds == {"K": "Hashable"}


def test_parse_nongeneric_struct_has_empty_type_vars():
    parse_string("struct Point { x: int y: int }")
    struct = GLOBAL_STRUCTS["Point"]
    assert struct.type_vars == []


# ---------------------------------------------------------------------------
# Resolver
# ---------------------------------------------------------------------------
def test_resolve_generic_struct_new():
    ops = resolve_string("struct Box[T] { value: T } 42 Box")
    struct_new_ops = find_ops(ops, OpKind.STRUCT_NEW)
    assert len(struct_new_ops) == 1
    assert isinstance(struct_new_ops[0].value, Struct)
    assert struct_new_ops[0].value.type_vars == ["T"]


# ---------------------------------------------------------------------------
# Type checker
# ---------------------------------------------------------------------------
def test_typecheck_generic_struct_infers_type():
    code = "struct Box[T] { value: T } 42 Box"
    sig = typecheck_string(code)
    assert sig.return_types == ["Box[int]"]


def test_typecheck_generic_struct_str():
    code = 'struct Box[T] { value: T } "hello" Box'
    sig = typecheck_string(code)
    assert sig.return_types == ["Box[str]"]


def test_typecheck_generic_struct_bool():
    code = "struct Box[T] { value: T } true Box"
    sig = typecheck_string(code)
    assert sig.return_types == ["Box[bool]"]


def test_typecheck_generic_struct_getter():
    code = "struct Box[T] { value: T } 42 Box .value"
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


def test_typecheck_generic_struct_setter():
    code = "struct Box[T] { value: T } 42 Box = b 99 b ->value"
    sig = typecheck_string(code)
    assert sig.return_types == []


def test_typecheck_generic_struct_two_fields():
    # Stack order: second pushed first, first pushed last (on top)
    code = "struct Pair[A B] { first: A second: B } true 42 Pair"
    sig = typecheck_string(code)
    assert sig.return_types == ["Pair[int bool]"]


def test_typecheck_generic_struct_two_fields_getter():
    code = "struct Pair[A B] { first: A second: B } true 42 Pair .second"
    sig = typecheck_string(code)
    assert sig.return_types == ["bool"]


def test_typecheck_generic_struct_nested():
    code = """
    struct Box[T] { value: T }
    struct Wrapper[U] { inner: U }
    42 Box Wrapper
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["Wrapper[Box[int]]"]


def test_typecheck_generic_struct_in_function():
    code = """
    struct Box[T] { value: T }
    fn unbox[T] b:Box[T] -> T { b .value }
    42 Box unbox
    """
    sig = typecheck_string(code)
    assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# Bytecode
# ---------------------------------------------------------------------------
def test_bytecode_generic_struct():
    code = "struct Box[T] { value: T } 42 Box"
    program = compile_string(code)
    assert program.bytecode


# ---------------------------------------------------------------------------
# Emitter
# ---------------------------------------------------------------------------
def test_emit_generic_struct():
    code = "struct Box[T] { value: T } 42 Box drop"
    asm = emit_string(code)
    assert "_start:" in asm
