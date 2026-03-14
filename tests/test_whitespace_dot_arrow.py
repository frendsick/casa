"""Tests for disallowing whitespace before dot (.) and arrow (->) syntax."""

import pytest

from casa.common import GLOBAL_FUNCTIONS, OpKind
from casa.error import CasaErrorCollection, ErrorKind
from tests.conftest import parse_string


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def find_ops(ops, kind):
    return [op for op in ops if op.kind == kind]


# ---------------------------------------------------------------------------
# Dot syntax: no whitespace before dot
# ---------------------------------------------------------------------------
class TestDotNoWhitespace:
    """Dot method call must be directly adjacent to the preceding token."""

    def test_dot_adjacent_parses(self):
        """person.name (no space before dot) should parse as METHOD_CALL."""
        ops = parse_string("x.name")
        method_ops = find_ops(ops, OpKind.METHOD_CALL)
        assert len(method_ops) == 1
        assert method_ops[0].value == "name"

    def test_dot_with_space_raises(self):
        """x .name (space before dot) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("x .name")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_dot_with_tab_raises(self):
        """x\t.name (tab before dot) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("x\t.name")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_dot_with_newline_raises(self):
        """x\\n.name (newline before dot) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("x\n.name")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_dot_chained_adjacent_parses(self):
        """x.foo.bar (chained dots, no spaces) should parse two METHOD_CALLs."""
        ops = parse_string("x.foo.bar")
        method_ops = find_ops(ops, OpKind.METHOD_CALL)
        assert len(method_ops) == 2
        assert method_ops[0].value == "foo"
        assert method_ops[1].value == "bar"

    def test_dot_chained_space_before_second_dot_raises(self):
        """x.foo .bar (space before second dot) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("x.foo .bar")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_dot_in_function_body_adjacent_parses(self):
        """Dot accessor inside function body works when adjacent."""
        code = """
        struct Person { name: str }
        fn get_name person:Person -> str { person.name }
        """
        parse_string(code)
        fn = GLOBAL_FUNCTIONS["get_name"]
        method_ops = find_ops(fn.ops, OpKind.METHOD_CALL)
        assert len(method_ops) == 1
        assert method_ops[0].value == "name"

    def test_dot_in_function_body_space_raises(self):
        """Dot accessor with space inside function body should raise error."""
        code = """
        struct Person { name: str }
        fn get_name person:Person -> str { person .name }
        """
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string(code)
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX


# ---------------------------------------------------------------------------
# Arrow syntax: no whitespace before arrow
# ---------------------------------------------------------------------------
class TestArrowNoWhitespace:
    """Arrow setter must be directly adjacent to the preceding token."""

    def test_arrow_adjacent_parses(self):
        """person->name (no space before arrow) should parse as METHOD_CALL."""
        ops = parse_string("42 x->name")
        method_ops = find_ops(ops, OpKind.METHOD_CALL)
        assert len(method_ops) == 1
        assert method_ops[0].value == "set_name"

    def test_arrow_with_space_raises(self):
        """x ->name (space before arrow) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("42 x ->name")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_arrow_with_tab_raises(self):
        """x\t->name (tab before arrow) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("42 x\t->name")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_arrow_with_newline_raises(self):
        """x\\n->name (newline before arrow) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("42 x\n->name")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_arrow_in_function_body_adjacent_parses(self):
        """Arrow setter inside function body works when adjacent."""
        code = """
        struct Person { name: str age: int }
        fn set_person_name person:Person name:str -> Person {
            name person->name
        }
        """
        parse_string(code)
        fn = GLOBAL_FUNCTIONS["set_person_name"]
        method_ops = find_ops(fn.ops, OpKind.METHOD_CALL)
        assert len(method_ops) == 1
        assert method_ops[0].value == "set_name"

    def test_arrow_in_function_body_space_raises(self):
        """Arrow setter with space inside function body should raise error."""
        code = """
        struct Person { name: str age: int }
        fn set_person_name person:Person name:str -> Person {
            name person ->name
        }
        """
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string(code)
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX


# ---------------------------------------------------------------------------
# Function signature arrow is unaffected
# ---------------------------------------------------------------------------
class TestFunctionSignatureArrow:
    """The -> in function signatures must continue to work with spaces."""

    def test_fn_signature_arrow_with_spaces(self):
        """fn foo a:int -> int should still parse correctly."""
        parse_string("fn foo a:int -> int { a }")
        assert "foo" in GLOBAL_FUNCTIONS
        fn = GLOBAL_FUNCTIONS["foo"]
        assert fn.signature is not None
        assert fn.signature.return_types == ["int"]

    def test_fn_signature_arrow_multiple_params(self):
        """fn bar a:int b:int -> int should still parse correctly."""
        parse_string("fn bar a:int b:int -> int { a b + }")
        assert "bar" in GLOBAL_FUNCTIONS

    def test_fn_signature_no_return(self):
        """fn baz a:int should still parse correctly (no ->)."""
        parse_string("fn baz a:int { a drop }")
        assert "baz" in GLOBAL_FUNCTIONS

    def test_fn_signature_multiple_returns(self):
        """fn qux a:int -> int int should still parse correctly."""
        parse_string("fn qux a:int -> int int { a a }")
        assert "qux" in GLOBAL_FUNCTIONS
        fn = GLOBAL_FUNCTIONS["qux"]
        assert fn.signature.return_types == ["int", "int"]


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------
class TestEdgeCases:
    """Edge cases for whitespace detection around dot and arrow."""

    def test_dot_after_int_literal(self):
        """42.method (dot after int literal, adjacent) should parse."""
        ops = parse_string("42.method")
        method_ops = find_ops(ops, OpKind.METHOD_CALL)
        assert len(method_ops) == 1
        assert method_ops[0].value == "method"

    def test_dot_after_string_literal(self):
        """\"hello\".method (dot after string literal, adjacent) should parse."""
        ops = parse_string('"hello".method')
        method_ops = find_ops(ops, OpKind.METHOD_CALL)
        assert len(method_ops) == 1
        assert method_ops[0].value == "method"

    def test_dot_after_close_paren_adjacent(self):
        """(int).method (dot after type cast, adjacent) should parse."""
        ops = parse_string("42 (int).method")
        method_ops = find_ops(ops, OpKind.METHOD_CALL)
        assert len(method_ops) == 1

    def test_arrow_standalone_is_not_setter(self):
        """Standalone -> without preceding token context is handled differently in fn signatures."""
        parse_string("fn id a:int -> int { a }")
        assert "id" in GLOBAL_FUNCTIONS

    def test_multiple_spaces_before_dot_raises(self):
        """x   .name (multiple spaces before dot) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("x   .name")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_multiple_spaces_before_arrow_raises(self):
        """x   ->name (multiple spaces before arrow) should raise a syntax error."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("42 x   ->name")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_error_message_mentions_whitespace(self):
        """Error message should mention whitespace or spacing."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("x .name")
        error_message = exc_info.value.errors[0].message.lower()
        assert "whitespace" in error_message or "space" in error_message
