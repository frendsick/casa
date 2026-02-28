"""Tests for type annotations on variable assignments (= name:type)."""

import pytest

from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_VARIABLES,
    OpKind,
    Variable,
)
from casa.error import WARNINGS, CasaErrorCollection, ErrorKind, WarningKind
from tests.conftest import parse_string, resolve_string, typecheck_string


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def find_ops(ops: list, kind: OpKind) -> list:
    return [op for op in ops if op.kind == kind]


# ---------------------------------------------------------------------------
# Parser tests
# ---------------------------------------------------------------------------
class TestParserTypeAnnotation:
    """Parser correctly attaches type_annotation to ASSIGN_VARIABLE ops."""

    def test_annotation_int(self):
        """= x:int produces Op with type_annotation='int'."""
        ops = parse_string("42 = x:int")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert len(assign_ops) == 1
        assert assign_ops[0].value == "x"
        assert assign_ops[0].type_annotation == "int"

    def test_annotation_str(self):
        """= x:str produces Op with type_annotation='str'."""
        ops = parse_string('"hello" = x:str')
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert assign_ops[0].type_annotation == "str"

    def test_annotation_bool(self):
        """= x:bool produces Op with type_annotation='bool'."""
        ops = parse_string("true = x:bool")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert assign_ops[0].type_annotation == "bool"

    def test_annotation_option_int(self):
        """= x:option[int] produces Op with type_annotation='option[int]'."""
        ops = parse_string("42 some = x:option[int]")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert assign_ops[0].type_annotation == "option[int]"

    def test_annotation_bare_option(self):
        """= x:option produces Op with type_annotation='option'."""
        ops = parse_string("none = x:option")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert assign_ops[0].type_annotation == "option"

    def test_annotation_generic_list(self):
        """= x:List[int] produces Op with type_annotation='List[int]'."""
        ops = parse_string("0 = x:List[int]")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert assign_ops[0].type_annotation == "List[int]"

    def test_annotation_ptr(self):
        """= x:ptr produces Op with type_annotation='ptr'."""
        ops = parse_string("0 = x:ptr")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert assign_ops[0].type_annotation == "ptr"

    def test_no_annotation(self):
        """= x (no annotation) produces Op with type_annotation=None."""
        ops = parse_string("42 = x")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert len(assign_ops) == 1
        assert assign_ops[0].value == "x"
        assert assign_ops[0].type_annotation is None

    def test_annotation_in_function(self):
        """Type annotation works inside function bodies."""
        parse_string("fn foo { 42 = x:int x drop }")
        fn = GLOBAL_FUNCTIONS["foo"]
        assign_ops = find_ops(fn.ops, OpKind.ASSIGN_VARIABLE)
        # Function params are also ASSIGN_VARIABLE, but foo has none
        assert any(op.value == "x" and op.type_annotation == "int" for op in assign_ops)

    def test_annotation_array_type(self):
        """= x:array[int] produces Op with type_annotation='array[int]'."""
        ops = parse_string("[1, 2] = x:array[int]")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert assign_ops[0].type_annotation == "array[int]"

    def test_annotation_char(self):
        """= x:char produces Op with type_annotation='char'."""
        ops = parse_string("'a' = x:char")
        assign_ops = find_ops(ops, OpKind.ASSIGN_VARIABLE)
        assert assign_ops[0].type_annotation == "char"

    def test_no_annotation_on_increment(self):
        """Increment (+=) does not support type annotations."""
        ops = parse_string("42 = x 1 += x")
        inc_ops = find_ops(ops, OpKind.ASSIGN_INCREMENT)
        assert len(inc_ops) == 1
        assert (
            not hasattr(inc_ops[0], "type_annotation")
            or inc_ops[0].type_annotation is None
        )

    def test_no_annotation_on_decrement(self):
        """Decrement (-=) does not support type annotations."""
        ops = parse_string("42 = x 1 -= x")
        dec_ops = find_ops(ops, OpKind.ASSIGN_DECREMENT)
        assert len(dec_ops) == 1
        assert (
            not hasattr(dec_ops[0], "type_annotation")
            or dec_ops[0].type_annotation is None
        )


# ---------------------------------------------------------------------------
# Type checker tests
# ---------------------------------------------------------------------------
class TestTypecheckerTypeAnnotation:
    """Type checker uses type_annotation for variable type inference."""

    def test_basic_annotation_int(self):
        """42 = x:int assigns x with type int."""
        sig = typecheck_string("42 = x:int x")
        assert sig.return_types == ["int"]

    def test_basic_annotation_str(self):
        """'hello' = x:str assigns x with type str."""
        sig = typecheck_string('"hello" = x:str x')
        assert sig.return_types == ["str"]

    def test_basic_annotation_bool(self):
        """true = x:bool assigns x with type bool."""
        sig = typecheck_string("true = x:bool x")
        assert sig.return_types == ["bool"]

    def test_annotation_matches_stack_type(self):
        """Annotation that matches stack type works fine."""
        sig = typecheck_string("42 = x:int x")
        assert sig.return_types == ["int"]

    def test_narrow_any_to_int(self):
        """Type annotation narrows 'any' to concrete type."""
        # Use type cast to get 'any' on stack, then annotate
        sig = typecheck_string("42 (any) = x:int x")
        assert sig.return_types == ["int"]

    def test_narrow_any_to_str(self):
        """Type annotation narrows 'any' to str."""
        sig = typecheck_string('"hello" (any) = x:str x')
        assert sig.return_types == ["str"]

    def test_narrow_bare_option_to_option_int(self):
        """Type annotation narrows bare 'option' to 'option[int]'."""
        sig = typecheck_string("none = x:option[int] x")
        assert sig.return_types == ["option[int]"]

    def test_type_mismatch_raises(self):
        """Annotating str value as int raises TYPE_MISMATCH."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string('"hello" = x:int')
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_type_mismatch_int_annotated_bool(self):
        """Annotating int value as bool raises TYPE_MISMATCH."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("42 = x:bool")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_type_mismatch_bool_annotated_str(self):
        """Annotating bool value as str raises TYPE_MISMATCH."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("true = x:str")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_reassignment_keeps_annotated_type(self):
        """Variable typed via annotation keeps its type on reassignment."""
        sig = typecheck_string("42 = x:int 99 = x x")
        assert sig.return_types == ["int"]

    def test_reassignment_type_mismatch(self):
        """Reassigning annotated variable with wrong type raises error."""
        with pytest.raises(CasaErrorCollection):
            typecheck_string('42 = x:int "hello" = x')

    def test_annotation_in_function_body(self):
        """Type annotation works in function-local variables."""
        sig = typecheck_string("fn foo -> int { 42 = x:int x } foo")
        assert sig.return_types == ["int"]

    def test_annotation_option_int_with_some(self):
        """42 some = x:option[int] assigns x with type option[int]."""
        sig = typecheck_string("42 some = x:option[int] x")
        assert sig.return_types == ["option[int]"]

    def test_without_annotation_still_works(self):
        """Assignment without annotation still infers type normally."""
        sig = typecheck_string("42 = x x")
        assert sig.return_types == ["int"]

    def test_narrow_any_in_function(self):
        """Type annotation narrows 'any' inside a function body."""
        sig = typecheck_string("fn foo -> int { 42 (any) = x:int x } foo")
        assert sig.return_types == ["int"]

    def test_annotation_with_struct_type(self):
        """Type annotation works with user-defined struct types."""
        code = """
        struct Point { x: int y: int }
        0 0 Point = p:Point
        p
        """
        sig = typecheck_string(code)
        assert sig.return_types == ["Point"]


class TestTypecheckerTypeAnnotationWarnings:
    """Type annotation warns when annotation loses type information."""

    def test_warn_annotation_any(self):
        """Annotating as any warns about losing type information."""
        typecheck_string("42 = x:any")
        assert len(WARNINGS) == 1
        assert WARNINGS[0].kind == WarningKind.LOSSY_TYPE_ANNOTATION
        assert "any" in WARNINGS[0].message

    def test_warn_bare_option_loses_param(self):
        """Annotating option[int] as bare option warns."""
        typecheck_string("42 some = x:option")
        assert len(WARNINGS) == 1
        assert WARNINGS[0].kind == WarningKind.LOSSY_TYPE_ANNOTATION
        assert "option" in WARNINGS[0].message

    def test_warn_bare_array_loses_param(self):
        """Annotating array[int] as bare array warns."""
        typecheck_string("[1, 2] = x:array")
        assert len(WARNINGS) == 1
        assert WARNINGS[0].kind == WarningKind.LOSSY_TYPE_ANNOTATION
        assert "array" in WARNINGS[0].message

    def test_no_warn_matching_types(self):
        """No warning when annotation matches stack type."""
        typecheck_string("42 = x:int")
        assert len(WARNINGS) == 0

    def test_no_warn_narrowing_option(self):
        """No warning when narrowing bare option to option[int]."""
        typecheck_string("none = x:option[int]")
        assert len(WARNINGS) == 0

    def test_no_warn_narrowing_any(self):
        """No warning when narrowing any to concrete type."""
        typecheck_string("42 (any) = x:int")
        assert len(WARNINGS) == 0
