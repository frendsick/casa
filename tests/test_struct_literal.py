"""Tests for struct literal construction and struct match destructuring."""

import pytest

from casa.common import (
    Op,
    OpKind,
    StructLiteral,
    StructPattern,
)
from casa.error import CasaErrorCollection, ErrorKind
from tests.conftest import (
    emit_string,
    parse_string,
    typecheck_string,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def find_ops(ops: list[Op], kind: OpKind) -> list[Op]:
    return [op for op in ops if op.kind == kind]


# ---------------------------------------------------------------------------
# Parser — struct literal construction
# ---------------------------------------------------------------------------
class TestParseStructLiteral:
    def test_basic_struct_literal(self):
        ops = parse_string("""\
struct Point { x: int y: int }
Point { x: 1 y: 2 }""")
        literals = find_ops(ops, OpKind.STRUCT_LITERAL)
        assert len(literals) == 1
        literal = literals[0].value
        assert isinstance(literal, StructLiteral)
        assert literal.struct.name == "Point"
        assert literal.field_order == ["x", "y"]

    def test_reverse_field_order(self):
        ops = parse_string("""\
struct Point { x: int y: int }
Point { y: 2 x: 1 }""")
        literal = find_ops(ops, OpKind.STRUCT_LITERAL)[0].value
        assert literal.field_order == ["y", "x"]

    def test_missing_field_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("""\
struct Point { x: int y: int }
Point { x: 1 }""")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_unknown_field_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("""\
struct Point { x: int y: int }
Point { z: 1 y: 2 }""")
        assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME

    def test_unknown_field_mid_literal_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("""\
struct Point { x: int y: int }
Point { x: 1 z: 2 }""")
        assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME

    def test_duplicate_field_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("""\
struct Point { x: int y: int }
Point { x: 1 x: 2 y: 3 }""")
        assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME

    def test_empty_literal_error(self):
        with pytest.raises(CasaErrorCollection):
            parse_string("""\
struct Point { x: int y: int }
Point { }""")

    def test_field_value_with_expression(self):
        ops = parse_string("""\
struct Point { x: int y: int }
Point { x: 1 2 + y: 3 }""")
        literals = find_ops(ops, OpKind.STRUCT_LITERAL)
        assert len(literals) == 1

    def test_missing_value_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("""\
struct Point { x: int y: int }
Point { x: y: 1 }""")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX


# ---------------------------------------------------------------------------
# Parser — struct match pattern
# ---------------------------------------------------------------------------
class TestParseStructMatchPattern:
    def test_basic_struct_pattern(self):
        ops = parse_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { x: px y: py } => px py +
end""")
        arms = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arms) == 1
        pattern = arms[0].value
        assert isinstance(pattern, StructPattern)
        assert pattern.struct_name == "Point"
        assert pattern.bindings == {"x": "px", "y": "py"}

    def test_partial_binding(self):
        ops = parse_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { x: px } => px
end""")
        pattern = find_ops(ops, OpKind.MATCH_ARM)[0].value
        assert isinstance(pattern, StructPattern)
        assert pattern.bindings == {"x": "px"}

    def test_wildcard_arm(self):
        ops = parse_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    _ => 0
end""")
        arms = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arms) == 1
        assert arms[0].value.is_wildcard

    def test_unknown_struct_in_pattern_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Rect { x: px } => px
end""")
        assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME

    def test_unknown_field_in_pattern_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { z: pz } => pz
end""")
        assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME

    def test_duplicate_field_in_pattern_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { x: a x: b } => a
end""")
        assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME


# ---------------------------------------------------------------------------
# Type checker — struct literal
# ---------------------------------------------------------------------------
class TestTypecheckStructLiteral:
    def test_basic_type(self):
        sig = typecheck_string("""\
struct Point { x: int y: int }
Point { x: 1 y: 2 }""")
        assert sig.return_types == ["Point"]

    def test_reverse_order_type(self):
        sig = typecheck_string("""\
struct Point { x: int y: int }
Point { y: 2 x: 1 }""")
        assert sig.return_types == ["Point"]

    def test_wrong_field_type_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("""\
struct Point { x: int y: int }
Point { x: "hello" y: 2 }""")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_generic_struct_literal(self):
        sig = typecheck_string("""\
struct Box[T] { value: T }
Box { value: 42 }""")
        assert sig.return_types == ["Box[int]"]

    def test_generic_struct_literal_str(self):
        sig = typecheck_string("""\
struct Box[T] { value: T }
Box { value: "hello" }""")
        assert sig.return_types == ["Box[str]"]


# ---------------------------------------------------------------------------
# Type checker — struct match
# ---------------------------------------------------------------------------
class TestTypecheckStructMatch:
    def test_struct_match_exhaustive(self):
        typecheck_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { x: px y: py } => px py +
end
drop""")

    def test_struct_match_wildcard_exhaustive(self):
        typecheck_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    _ => 0
end
drop""")

    def test_struct_match_non_exhaustive_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
end""")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_struct_match_wrong_struct_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("""\
struct Point { x: int y: int }
struct Rect { w: int h: int }
1 2 Point = p
p match
    Rect { w: rw } => rw
end""")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_struct_match_binding_types(self):
        sig = typecheck_string("""\
struct Person { name: str age: int }
18 "Alice" Person = p
p match
    Person { name: n age: a } => n a
end""")
        assert sig.return_types == ["str", "int"]

    def test_struct_match_partial_binding(self):
        sig = typecheck_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { x: px } => px
end""")
        assert sig.return_types == ["int"]

    def test_struct_match_duplicate_arm_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { x: px } => px
    Point { y: py } => py
end""")
        assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME

    def test_match_on_unsupported_type_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("""\
42 (ptr) match
    _ => 0
end""")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH


# ---------------------------------------------------------------------------
# Bytecode / integration — struct literal
# ---------------------------------------------------------------------------
class TestCompileStructLiteral:
    def test_compile_struct_literal(self):
        emit_string("""\
struct Point { x: int y: int }
Point { x: 1 y: 2 } = p
p.x drop""")

    def test_compile_struct_literal_reverse_order(self):
        emit_string("""\
struct Point { x: int y: int }
Point { y: 99 x: 42 } = p
p.x drop""")

    def test_compile_struct_match(self):
        emit_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { x: px y: py } => px py +
end
drop""")

    def test_compile_struct_match_as_expression(self):
        emit_string("""\
struct Point { x: int y: int }
1 2 Point = p
p match
    Point { x: px } => px
end
drop""")


# ---------------------------------------------------------------------------
# Backward compatibility
# ---------------------------------------------------------------------------
class TestBackwardCompatibility:
    def test_stack_based_construction_still_works(self):
        sig = typecheck_string("""\
struct Point { x: int y: int }
1 2 Point""")
        assert sig.return_types == ["Point"]

    def test_stack_based_and_literal_coexist(self):
        sig = typecheck_string("""\
struct Point { x: int y: int }
1 2 Point = p1
Point { x: 3 y: 4 } = p2
p1.x p2.x +""")
        assert sig.return_types == ["int"]
