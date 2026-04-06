"""Tests for the enum feature: parsing, type checking, bytecode, and end-to-end."""

import os
import subprocess

import pytest

from casa.common import (
    GLOBAL_ENUMS,
    CasaEnum,
    EnumVariant,
    InstKind,
    LiteralPattern,
    OpKind,
    Program,
)
from casa.error import CasaErrorCollection, ErrorKind
from tests.conftest import (
    compile_string,
    parse_string,
    resolve_string,
    typecheck_string,
)

CASA_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
COLOR_ENUM = "enum Color { Red Green Blue }\n"
DIRECTION_ENUM = "enum Direction { North South East West }\n"
SHAPE_ENUM = "enum Shape { Circle(int) Rectangle(int int) Point }\n"
OPTION_ENUM = "enum Option[T] { None Some(T) }\n"
RESULT_ENUM = "enum Result[T E] { Error(E) Ok(T) }\n"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def find_ops(ops: list, kind: OpKind) -> list:
    return [op for op in ops if op.kind == kind]


# ---------------------------------------------------------------------------
# Enum declaration parsing
# ---------------------------------------------------------------------------
class TestEnumParsing:
    def test_parse_enum_registers_globally(self):
        parse_string("enum Color { Red Green Blue }")
        assert "Color" in GLOBAL_ENUMS

    def test_parse_enum_is_casa_enum(self):
        parse_string("enum Color { Red Green Blue }")
        assert isinstance(GLOBAL_ENUMS["Color"], CasaEnum)

    def test_parse_enum_name(self):
        parse_string("enum Color { Red Green Blue }")
        assert GLOBAL_ENUMS["Color"].name == "Color"

    def test_parse_enum_variant_count(self):
        parse_string("enum Color { Red Green Blue }")
        assert len(GLOBAL_ENUMS["Color"].variants) == 3

    def test_parse_enum_variant_names(self):
        parse_string("enum Color { Red Green Blue }")
        assert GLOBAL_ENUMS["Color"].variants == ["Red", "Green", "Blue"]

    def test_parse_enum_has_location(self):
        parse_string("enum Color { Red Green Blue }")
        assert GLOBAL_ENUMS["Color"].location is not None

    def test_parse_enum_single_variant(self):
        parse_string("enum Unit { Only }")
        assert GLOBAL_ENUMS["Unit"].variants == ["Only"]

    def test_parse_enum_many_variants(self):
        parse_string("enum Direction { North South East West }")
        assert len(GLOBAL_ENUMS["Direction"].variants) == 4

    def test_parse_duplicate_enum_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string(
                "enum Color { Red Green Blue }\n" "enum Color { Cyan Magenta Yellow }\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME

    def test_parse_enum_empty_raises(self):
        with pytest.raises(CasaErrorCollection):
            parse_string("enum Empty { }")

    def test_parse_duplicate_variant_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("enum Dup { A B A }\n")
        assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME

    def test_parse_enum_name_conflicts_with_struct_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("struct Color { val: int }\nenum Color { Red Green }\n")
        assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME

    def test_parse_enum_unclosed_block_raises(self):
        with pytest.raises(CasaErrorCollection):
            parse_string("enum Color { Red Green Blue")

    def test_parse_enum_with_inner_types(self):
        parse_string(SHAPE_ENUM)
        shape = GLOBAL_ENUMS["Shape"]
        assert shape.variant_types["Circle"] == ["int"]
        assert shape.variant_types["Rectangle"] == ["int", "int"]
        assert shape.variant_types["Point"] == []

    def test_parse_enum_has_inner_values(self):
        parse_string(SHAPE_ENUM)
        assert GLOBAL_ENUMS["Shape"].has_inner_values is True

    def test_parse_plain_enum_no_inner_values(self):
        parse_string(COLOR_ENUM)
        assert GLOBAL_ENUMS["Color"].has_inner_values is False

    def test_parse_generic_enum_type_vars(self):
        parse_string(OPTION_ENUM)
        assert GLOBAL_ENUMS["Option"].type_vars == ["T"]

    def test_parse_generic_enum_two_type_vars(self):
        parse_string(RESULT_ENUM)
        assert GLOBAL_ENUMS["Result"].type_vars == ["T", "E"]

    def test_parse_generic_enum_type_var_order_preserved(self):
        parse_string("enum Pair[A B] { Left(A) Right(B) }\n")
        assert GLOBAL_ENUMS["Pair"].type_vars == ["A", "B"]

    def test_parse_empty_inner_types_raises(self):
        with pytest.raises(CasaErrorCollection):
            parse_string("enum Bad { Variant() }\n")

    def test_parse_duplicate_type_var_raises(self):
        with pytest.raises(CasaErrorCollection):
            parse_string("enum Bad[T T] { A(T) }\n")

    def test_parse_match_destructuring_bindings(self):
        ops = resolve_string(
            SHAPE_ENUM
            + "10 Shape::Circle match\n"
            + "    Shape::Circle(radius) => radius\n"
            + "    Shape::Rectangle(width height) => width\n"
            + "    Shape::Point => 0\n"
            + "end\n"
        )
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert arm_ops[0].value.bindings == ["radius"]
        assert arm_ops[1].value.bindings == ["width", "height"]
        assert arm_ops[2].value.bindings == []


# ---------------------------------------------------------------------------
# Match arm: bare variant hint
# ---------------------------------------------------------------------------
class TestMatchBareVariantHint:
    def test_literal_in_enum_match_rejected_by_typechecker(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                "enum Color { Red Green Blue }\n"
                "Color::Red match\n"
                "    42 => 1\n"
                "    _ => 2\n"
                "end\n"
            )
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH

    def test_bare_identifier_parsed_as_match_arm(self):
        """Bare identifiers are parsed as match arms and deferred to type checker."""
        ops = parse_string(
            "enum Color { Red Green Blue }\n"
            "Color::Red match\n"
            "    Red => 1\n"
            "    Color::Green => 2\n"
            "    Color::Blue => 3\n"
            "end\n"
        )
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arm_ops) == 3

    def test_qualified_variant_still_works(self):
        ops = parse_string(
            "enum Color { Red Green Blue }\n"
            "Color::Red match\n"
            "    Color::Red => 1\n"
            "    Color::Green => 2\n"
            "    Color::Blue => 3\n"
            "end\n"
        )
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arm_ops) == 3


# ---------------------------------------------------------------------------
# Match block parsing
# ---------------------------------------------------------------------------
class TestMatchParsing:
    def test_parse_match_produces_match_ops(self):
        code = (
            "enum Color { Red Green Blue }\n"
            "Color::Red match\n"
            "    Color::Red => 1\n"
            "    Color::Green => 2\n"
            "    Color::Blue => 3\n"
            "end\n"
        )
        ops = parse_string(code)
        kinds = [op.kind for op in ops]
        assert OpKind.MATCH_START in kinds
        assert OpKind.MATCH_ARM in kinds
        assert OpKind.MATCH_END in kinds

    def test_parse_match_arm_count(self):
        code = (
            "enum Color { Red Green Blue }\n"
            "Color::Red match\n"
            "    Color::Red => 1\n"
            "    Color::Green => 2\n"
            "    Color::Blue => 3\n"
            "end\n"
        )
        ops = parse_string(code)
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arm_ops) == 3


# ---------------------------------------------------------------------------
# Braced match arm bodies
# ---------------------------------------------------------------------------
class TestBracedMatchArm:
    def test_braced_arm_produces_correct_ops(self):
        code = (
            COLOR_ENUM + "Color::Red match\n"
            "    Color::Red => { 1 }\n"
            "    Color::Green => { 2 }\n"
            "    Color::Blue => { 3 }\n"
            "end\n"
        )
        ops = parse_string(code)
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arm_ops) == 3

    def test_braced_arm_multiline_body(self):
        code = (
            COLOR_ENUM + "Color::Red match\n"
            "    Color::Red => {\n"
            '        "red" print\n'
            '        "\\n" print\n'
            "    }\n"
            '    Color::Green => "green"\n'
            '    Color::Blue => "blue"\n'
            "end\n"
        )
        ops = parse_string(code)
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arm_ops) == 3
        push_str_ops = find_ops(ops, OpKind.PUSH_STR)
        assert any(op.value == "red" for op in push_str_ops)

    def test_mixed_braced_and_unbraced_arms(self):
        code = (
            COLOR_ENUM + "Color::Green match\n"
            "    Color::Red => 0\n"
            "    Color::Green => {\n"
            "        1\n"
            "    }\n"
            "    Color::Blue => 2\n"
            "end\n"
        )
        ops = parse_string(code)
        push_int_ops = find_ops(ops, OpKind.PUSH_INT)
        values = [op.value for op in push_int_ops]
        assert 0 in values
        assert 1 in values
        assert 2 in values

    def test_braced_arm_as_expression(self):
        code = (
            COLOR_ENUM + "fn color_code c:Color -> int {\n"
            "    c match\n"
            "        Color::Red => { 0 }\n"
            "        Color::Green => { 1 }\n"
            "        Color::Blue => { 2 }\n"
            "    end\n"
            "}\n"
        )
        typecheck_string(code)

    def test_braced_arm_with_destructuring(self):
        code = (
            SHAPE_ENUM + "10 Shape::Circle match\n"
            "    Shape::Circle(radius) => {\n"
            '        "r=" print\n'
            "        radius print\n"
            "    }\n"
            "    Shape::Rectangle(width height) => {\n"
            "        width height * print\n"
            "    }\n"
            '    Shape::Point => "point" print\n'
            "end\n"
        )
        ops = parse_string(code)
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arm_ops) == 3

    def test_empty_braced_arm_body(self):
        code = (
            COLOR_ENUM + "Color::Red match\n"
            "    Color::Red => {}\n"
            "    Color::Green => {}\n"
            "    Color::Blue => {}\n"
            "end\n"
        )
        ops = parse_string(code)
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arm_ops) == 3

    def test_unclosed_brace_in_arm_raises(self):
        code = (
            COLOR_ENUM + "Color::Red match\n"
            "    Color::Red => {\n"
            "        1\n"
            "    Color::Green => 2\n"
            "    Color::Blue => 3\n"
            "end\n"
        )
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string(code)
        assert exc_info.value.errors[0].kind == ErrorKind.UNMATCHED_BLOCK

    def test_multiline_unbraced_arm_raises(self):
        code = (
            COLOR_ENUM + "Color::Red match\n"
            '    Color::Red => "red" print\n'
            '        "\\n" print\n'
            "    Color::Green => 2\n"
            "    Color::Blue => 3\n"
            "end\n"
        )
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string(code)
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX
        assert "requires" in exc_info.value.errors[0].message

    def test_single_line_arm_body_on_next_line_is_valid(self):
        code = (
            COLOR_ENUM + "Color::Red match\n"
            "    Color::Red =>\n"
            "        1\n"
            "    Color::Green => 2\n"
            "    Color::Blue => 3\n"
            "end\n"
        )
        ops = parse_string(code)
        arm_ops = find_ops(ops, OpKind.MATCH_ARM)
        assert len(arm_ops) == 3


# ---------------------------------------------------------------------------
# Identifier resolution: Color::Red -> PUSH_ENUM_VARIANT
# ---------------------------------------------------------------------------
class TestEnumResolution:
    def test_resolve_enum_variant(self):
        ops = resolve_string("enum Color { Red Green Blue } Color::Red")
        variant_ops = find_ops(ops, OpKind.PUSH_ENUM_VARIANT)
        assert len(variant_ops) == 1

    def test_resolve_enum_variant_value_is_enum_variant(self):
        ops = resolve_string("enum Color { Red Green Blue } Color::Red")
        variant_ops = find_ops(ops, OpKind.PUSH_ENUM_VARIANT)
        assert isinstance(variant_ops[0].value, EnumVariant)

    def test_resolve_enum_variant_ordinal_first(self):
        ops = resolve_string("enum Color { Red Green Blue } Color::Red")
        variant_ops = find_ops(ops, OpKind.PUSH_ENUM_VARIANT)
        assert variant_ops[0].value.ordinal == 0

    def test_resolve_enum_variant_ordinal_second(self):
        ops = resolve_string("enum Color { Red Green Blue } Color::Green")
        variant_ops = find_ops(ops, OpKind.PUSH_ENUM_VARIANT)
        assert variant_ops[0].value.ordinal == 1

    def test_resolve_enum_variant_ordinal_third(self):
        ops = resolve_string("enum Color { Red Green Blue } Color::Blue")
        variant_ops = find_ops(ops, OpKind.PUSH_ENUM_VARIANT)
        assert variant_ops[0].value.ordinal == 2

    def test_resolve_enum_variant_enum_name(self):
        ops = resolve_string("enum Color { Red Green Blue } Color::Red")
        variant_ops = find_ops(ops, OpKind.PUSH_ENUM_VARIANT)
        assert variant_ops[0].value.enum_name == "Color"

    def test_resolve_enum_variant_variant_name(self):
        ops = resolve_string("enum Color { Red Green Blue } Color::Green")
        variant_ops = find_ops(ops, OpKind.PUSH_ENUM_VARIANT)
        assert variant_ops[0].value.variant_name == "Green"

    def test_resolve_undefined_enum_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            resolve_string("Nonexistent::Foo")
        assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME

    def test_resolve_undefined_variant_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            resolve_string("enum Color { Red Green Blue } Color::Yellow")
        assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME

    def test_resolve_multiple_variants(self):
        ops = resolve_string(COLOR_ENUM + "Color::Red Color::Green Color::Blue\n")
        variant_ops = find_ops(ops, OpKind.PUSH_ENUM_VARIANT)
        assert len(variant_ops) == 3
        ordinals = [op.value.ordinal for op in variant_ops]
        assert ordinals == [0, 1, 2]


# ---------------------------------------------------------------------------
# Type checking: enum variants
# ---------------------------------------------------------------------------
class TestEnumTypeChecking:
    def test_enum_variant_pushes_enum_type(self):
        sig = typecheck_string("enum Color { Red Green Blue } Color::Red")
        assert sig.return_types == ["Color"]

    def test_enum_eq_same_type(self):
        sig = typecheck_string(
            "enum Color { Red Green Blue } Color::Red Color::Green =="
        )
        assert sig.return_types == ["bool"]

    def test_enum_ne_same_type(self):
        sig = typecheck_string(
            "enum Color { Red Green Blue } Color::Red Color::Blue !="
        )
        assert sig.return_types == ["bool"]

    @pytest.mark.parametrize("op", ["<", "<=", ">", ">="])
    def test_enum_ordering_same_type(self, op):
        sig = typecheck_string(
            COLOR_ENUM + f"Color::Red Color::Blue {op}"
        )
        assert sig.return_types == ["bool"]

    def test_data_enum_ordering(self):
        sig = typecheck_string(SHAPE_ENUM + "Shape::Point Shape::Point <")
        assert sig.return_types == ["bool"]

    def test_enum_eq_different_types_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                "enum Color { Red Green Blue }\n"
                "enum Shape { Circle Square }\n"
                "Color::Red Shape::Circle ==\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_enum_assign_variable(self):
        sig = typecheck_string("enum Color { Red Green Blue } Color::Red = c c")
        assert sig.return_types == ["Color"]

    def test_enum_in_function_param(self):
        typecheck_string(
            "enum Color { Red Green Blue }\n"
            "fn is_red c:Color -> bool { c Color::Red == }\n"
            "Color::Red is_red\n"
        )

    def test_enum_int_comparison_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(COLOR_ENUM + "Color::Red 42 ==\n")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_enum_as_return_type(self):
        typecheck_string(
            COLOR_ENUM + "fn get_color -> Color { Color::Blue }\n" + "get_color\n"
        )

    def test_wrong_enum_type_in_function_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + DIRECTION_ENUM
                + "fn is_red c:Color -> bool { c Color::Red == }\n"
                + "Direction::North is_red\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_enum_print(self):
        sig = typecheck_string("enum Color { Red Green Blue } Color::Red print")
        assert sig.return_types == []


# ---------------------------------------------------------------------------
# Type checking: match exhaustiveness
# ---------------------------------------------------------------------------
class TestMatchExhaustiveness:
    def test_match_exhaustive_passes(self):
        typecheck_string(
            "enum Color { Red Green Blue }\n"
            "Color::Red match\n"
            "    Color::Red => 1\n"
            "    Color::Green => 2\n"
            "    Color::Blue => 3\n"
            "end\n"
            "drop\n"
        )

    def test_match_missing_variant_raises(self):
        with pytest.raises(CasaErrorCollection):
            typecheck_string(
                "enum Color { Red Green Blue }\n"
                "Color::Red match\n"
                "    Color::Red => 1\n"
                "    Color::Green => 2\n"
                "end\n"
                "drop\n"
            )

    def test_match_wrong_enum_variant_raises(self):
        with pytest.raises(CasaErrorCollection):
            typecheck_string(
                "enum Color { Red Green Blue }\n"
                "enum Shape { Circle Square }\n"
                "Color::Red match\n"
                "    Color::Red => 1\n"
                "    Color::Green => 2\n"
                "    Shape::Circle => 3\n"
                "end\n"
                "drop\n"
            )

    def test_match_arms_inconsistent_stack_raises(self):
        with pytest.raises(CasaErrorCollection):
            typecheck_string(
                "enum Color { Red Green Blue }\n"
                "Color::Red match\n"
                "    Color::Red => 1\n"
                '    Color::Green => "hello"\n'
                "    Color::Blue => 3\n"
                "end\n"
                "drop\n"
            )

    def test_match_as_expression(self):
        sig = typecheck_string(
            "enum Color { Red Green Blue }\n"
            "Color::Red match\n"
            "    Color::Red => 10\n"
            "    Color::Green => 20\n"
            "    Color::Blue => 30\n"
            "end\n"
        )
        assert sig.return_types == ["int"]

    def test_match_duplicate_arm_raises(self):
        with pytest.raises(CasaErrorCollection):
            typecheck_string(
                COLOR_ENUM
                + "Color::Red match\n"
                + "    Color::Red => 1\n"
                + "    Color::Red => 2\n"
                + "    Color::Green => 3\n"
                + "    Color::Blue => 4\n"
                + "end\n"
                + "drop\n"
            )

    def test_match_as_expression_str(self):
        sig = typecheck_string(
            COLOR_ENUM
            + "Color::Red match\n"
            + '    Color::Red => "red"\n'
            + '    Color::Green => "green"\n'
            + '    Color::Blue => "blue"\n'
            + "end\n"
        )
        assert sig.return_types == ["str"]

    def test_match_all_arms_drop_is_consistent(self):
        typecheck_string(
            COLOR_ENUM
            + "Color::Red match\n"
            + '    Color::Red => "red" print\n'
            + '    Color::Green => "green" print\n'
            + '    Color::Blue => "blue" print\n'
            + "end\n"
        )

    def test_match_arms_must_have_same_type(self):
        with pytest.raises(CasaErrorCollection):
            typecheck_string(
                "enum Color { Red Green Blue }\n"
                "Color::Red match\n"
                "    Color::Red => 1\n"
                '    Color::Green => "two"\n'
                "    Color::Blue => 3\n"
                "end\n"
            )

    def test_wildcard_only_match_with_stack_effect(self):
        """A match with only a wildcard arm that pushes a value should type check."""
        sig = typecheck_string(
            "enum Color { Red Green Blue }\n"
            "Color::Red match\n"
            "    _ => 42\n"
            "end\n"
        )
        assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# Match inside functions
# ---------------------------------------------------------------------------
class TestMatchInFunction:
    def test_match_inside_function(self):
        typecheck_string(
            COLOR_ENUM
            + "fn color_name c:Color -> str {\n"
            + "    c match\n"
            + '        Color::Red => "red"\n'
            + '        Color::Green => "green"\n'
            + '        Color::Blue => "blue"\n'
            + "    end\n"
            + "}\n"
            + "Color::Blue color_name\n"
        )

    def test_match_result_used_in_function(self):
        sig = typecheck_string(
            COLOR_ENUM
            + "fn color_value c:Color -> int {\n"
            + "    c match\n"
            + "        Color::Red => 0\n"
            + "        Color::Green => 1\n"
            + "        Color::Blue => 2\n"
            + "    end\n"
            + "}\n"
            + "Color::Green color_value\n"
        )
        assert sig.return_types == ["int"]


# ---------------------------------------------------------------------------
# Type checking: inner values and generics
# ---------------------------------------------------------------------------
class TestEnumInnerValuesTypeChecking:
    def test_data_variant_pops_inner_and_pushes_enum(self):
        sig = typecheck_string(SHAPE_ENUM + "10 Shape::Circle")
        assert sig.return_types == ["Shape"]

    def test_data_variant_two_inner_values(self):
        sig = typecheck_string(SHAPE_ENUM + "3 4 Shape::Rectangle")
        assert sig.return_types == ["Shape"]

    def test_no_data_variant_in_data_enum(self):
        sig = typecheck_string(SHAPE_ENUM + "Shape::Point")
        assert sig.return_types == ["Shape"]

    def test_generic_some_pushes_option_int(self):
        sig = typecheck_string(OPTION_ENUM + "42 Option::Some")
        assert sig.return_types == ["Option[int]"]

    def test_generic_some_pushes_option_str(self):
        sig = typecheck_string(OPTION_ENUM + '"hello" Option::Some')
        assert sig.return_types == ["Option[str]"]

    def test_generic_none_pushes_option_t(self):
        sig = typecheck_string(OPTION_ENUM + "Option::None")
        assert sig.return_types == ["Option[T]"]

    def test_result_ok_pushes_result(self):
        sig = typecheck_string(RESULT_ENUM + "42 Result::Ok")
        assert sig.return_types == ["Result[int E]"]

    def test_result_error_pushes_result(self):
        sig = typecheck_string(RESULT_ENUM + '"oops" Result::Error')
        assert sig.return_types == ["Result[T str]"]

    def test_data_enum_wrong_inner_type_raises(self):
        with pytest.raises(CasaErrorCollection):
            typecheck_string(SHAPE_ENUM + '"hello" Shape::Circle')

    def test_match_destructuring_binding_types(self):
        sig = typecheck_string(
            OPTION_ENUM
            + "42 Option::Some match\n"
            + "    Option::Some(value) => value\n"
            + "    Option::None => 0\n"
            + "end\n"
        )
        assert sig.return_types == ["int"]

    def test_match_destructuring_wrong_binding_count_raises(self):
        with pytest.raises(CasaErrorCollection):
            typecheck_string(
                SHAPE_ENUM
                + "10 Shape::Circle match\n"
                + "    Shape::Circle(a b) => a\n"
                + "    Shape::Rectangle(w h) => w\n"
                + "    Shape::Point => 0\n"
                + "end\n"
            )

    def test_data_enum_comparison(self):
        sig = typecheck_string(SHAPE_ENUM + "Shape::Point Shape::Point ==")
        assert sig.return_types == ["bool"]

    def test_generic_enum_in_function_return(self):
        sig = typecheck_string(
            OPTION_ENUM
            + "fn wrap x:int -> Option[int] { x Option::Some }\n"
            + "42 wrap\n"
        )
        assert sig.return_types == ["Option[int]"]


# ---------------------------------------------------------------------------
# Bytecode: PUSH_ENUM_VARIANT compiles to PUSH(ordinal)
# ---------------------------------------------------------------------------
def find_insts(program: Program, kind: InstKind, in_functions: bool = False):
    results = [i for i in program.bytecode if i.kind == kind]
    if in_functions:
        for fn_bc in program.functions.values():
            results += [i for i in fn_bc if i.kind == kind]
    return results


class TestEnumBytecode:
    def test_enum_variant_compiles_to_push_ordinal_0(self):
        program = compile_string(COLOR_ENUM + "Color::Red\n")
        pushes = find_insts(program, InstKind.PUSH)
        assert any(i.args == [0] for i in pushes)

    def test_enum_variant_compiles_to_push_ordinal_1(self):
        program = compile_string(COLOR_ENUM + "Color::Green\n")
        pushes = find_insts(program, InstKind.PUSH)
        assert any(i.args == [1] for i in pushes)

    def test_enum_variant_compiles_to_push_ordinal_2(self):
        program = compile_string(COLOR_ENUM + "Color::Blue\n")
        pushes = find_insts(program, InstKind.PUSH)
        assert any(i.args == [2] for i in pushes)

    def test_match_compiles_to_dup_eq_jump_pattern(self):
        program = compile_string(
            COLOR_ENUM
            + "Color::Red match\n"
            + "    Color::Red => 1 drop\n"
            + "    Color::Green => 2 drop\n"
            + "    Color::Blue => 3 drop\n"
            + "end\n"
        )
        # Last arm is optimized: no DUP/EQ/JUMP_NE (exhaustiveness guaranteed)
        assert len(find_insts(program, InstKind.DUP)) >= 2
        assert len(find_insts(program, InstKind.EQ)) >= 2
        assert len(find_insts(program, InstKind.JUMP_NE)) >= 2
        assert len(find_insts(program, InstKind.DROP)) >= 3
        assert len(find_insts(program, InstKind.LABEL)) >= 3

    def test_enum_print_compiles_to_print_int(self):
        program = compile_string(COLOR_ENUM + "Color::Red print\n")
        assert len(find_insts(program, InstKind.PRINT_INT)) >= 1

    def test_data_enum_variant_compiles_to_heap_alloc(self):
        program = compile_string(SHAPE_ENUM + "10 Shape::Circle\n")
        assert len(find_insts(program, InstKind.HEAP_ALLOC)) >= 1

    def test_data_enum_stores_ordinal(self):
        program = compile_string(SHAPE_ENUM + "10 Shape::Circle\n")
        assert len(find_insts(program, InstKind.STORE64)) >= 1

    def test_data_enum_match_loads_ordinal(self):
        program = compile_string(
            SHAPE_ENUM
            + "10 Shape::Circle match\n"
            + "    Shape::Circle(r) => r drop\n"
            + "    Shape::Rectangle(w h) => w drop\n"
            + "    Shape::Point => 0 drop\n"
            + "end\n"
        )
        assert len(find_insts(program, InstKind.LOAD64)) >= 1


# ---------------------------------------------------------------------------
# End-to-end: compile and run enum programs
# ---------------------------------------------------------------------------
class TestEnumEndToEnd:
    @pytest.fixture
    def run_casa(self, tmp_path):
        def _run(code: str) -> str:
            src = tmp_path / "test.casa"
            src.write_text(code)
            binary = tmp_path / "test"
            result = subprocess.run(
                ["python3", "casa.py", str(src), "-o", str(binary)],
                capture_output=True,
                text=True,
                cwd=CASA_ROOT,
                timeout=30,
            )
            if result.returncode != 0:
                pytest.fail(
                    f"Compilation failed:\nstdout: {result.stdout}\nstderr: {result.stderr}"
                )
            run_result = subprocess.run(
                [str(binary)],
                capture_output=True,
                text=True,
                timeout=10,
            )
            return run_result.stdout

        return _run

    def test_enum_print_ordinal(self, run_casa):
        output = run_casa("enum Color { Red Green Blue }\n" "Color::Red print\n")
        assert output == "0"

    def test_enum_print_second_variant(self, run_casa):
        output = run_casa("enum Color { Red Green Blue }\n" "Color::Green print\n")
        assert output == "1"

    def test_enum_eq_true(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n" "Color::Red Color::Red == print\n"
        )
        assert output == "true"

    def test_enum_eq_false(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n" "Color::Red Color::Green == print\n"
        )
        assert output == "false"

    def test_enum_ne(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n" "Color::Red Color::Blue != print\n"
        )
        assert output == "true"

    def test_enum_match_first_arm(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n"
            "Color::Red match\n"
            '    Color::Red => "red" print\n'
            '    Color::Green => "green" print\n'
            '    Color::Blue => "blue" print\n'
            "end\n"
        )
        assert output == "red"

    def test_enum_match_second_arm(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n"
            "Color::Green match\n"
            '    Color::Red => "red" print\n'
            '    Color::Green => "green" print\n'
            '    Color::Blue => "blue" print\n'
            "end\n"
        )
        assert output == "green"

    def test_enum_match_third_arm(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n"
            "Color::Blue match\n"
            '    Color::Red => "red" print\n'
            '    Color::Green => "green" print\n'
            '    Color::Blue => "blue" print\n'
            "end\n"
        )
        assert output == "blue"

    def test_enum_match_as_expression(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n"
            "Color::Green match\n"
            "    Color::Red => 10\n"
            "    Color::Green => 20\n"
            "    Color::Blue => 30\n"
            "end\n"
            "print\n"
        )
        assert output == "20"

    def test_enum_variable_match(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n"
            "Color::Blue = my_color\n"
            "my_color match\n"
            '    Color::Red => "r"\n'
            '    Color::Green => "g"\n'
            '    Color::Blue => "b"\n'
            "end\n"
            "print\n"
        )
        assert output == "b"

    def test_enum_in_function(self, run_casa):
        output = run_casa(
            "enum Color { Red Green Blue }\n"
            "fn color_name c:Color -> str {\n"
            "    c match\n"
            '        Color::Red => "red"\n'
            '        Color::Green => "green"\n'
            '        Color::Blue => "blue"\n'
            "    end\n"
            "}\n"
            "Color::Green color_name print\n"
        )
        assert output == "green"

    def test_enum_match_all_arms(self, run_casa):
        output = run_casa(
            DIRECTION_ENUM
            + "fn dir_str d:Direction -> str {\n"
            + "    d match\n"
            + '        Direction::North => "N"\n'
            + '        Direction::South => "S"\n'
            + '        Direction::East => "E"\n'
            + '        Direction::West => "W"\n'
            + "    end\n"
            + "}\n"
            + "Direction::North dir_str print\n"
            + "Direction::South dir_str print\n"
            + "Direction::East dir_str print\n"
            + "Direction::West dir_str print\n"
        )
        assert output == "NSEW"

    def test_enum_in_if_condition(self, run_casa):
        output = run_casa(
            COLOR_ENUM
            + "Color::Red = my_color\n"
            + 'if my_color Color::Red == then "yes" print fi\n'
        )
        assert output == "yes"

    def test_multiple_enums(self, run_casa):
        output = run_casa(
            COLOR_ENUM
            + DIRECTION_ENUM
            + "Color::Red print\n"
            + "Direction::West print\n"
        )
        assert output == "03"

    def test_data_enum_match_destructure(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "10 Shape::Circle match\n"
            + "    Shape::Circle(radius) => radius print\n"
            + "    Shape::Rectangle(width height) => width height * print\n"
            + '    Shape::Point => "point" print\n'
            + "end\n"
        )
        assert output == "10"

    def test_data_enum_match_second_arm(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "3 4 Shape::Rectangle match\n"
            + "    Shape::Circle(radius) => radius print\n"
            + "    Shape::Rectangle(width height) => width height * print\n"
            + '    Shape::Point => "point" print\n'
            + "end\n"
        )
        assert output == "12"

    def test_data_enum_match_no_data_arm(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "Shape::Point match\n"
            + "    Shape::Circle(radius) => radius print\n"
            + "    Shape::Rectangle(width height) => width height * print\n"
            + '    Shape::Point => "point" print\n'
            + "end\n"
        )
        assert output == "point"

    def test_braced_arm_end_to_end(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "10 Shape::Circle match\n"
            + "    Shape::Circle(radius) => {\n"
            + '        "r=" print\n'
            + "        radius print\n"
            + "    }\n"
            + "    Shape::Rectangle(width height) => {\n"
            + "        width height * print\n"
            + "    }\n"
            + '    Shape::Point => "point" print\n'
            + "end\n"
        )
        assert output == "r=10"

    def test_generic_enum_some(self, run_casa):
        output = run_casa(
            OPTION_ENUM
            + "42 Option::Some match\n"
            + "    Option::Some(value) => value print\n"
            + '    Option::None => "none" print\n'
            + "end\n"
        )
        assert output == "42"

    def test_generic_enum_none(self, run_casa):
        output = run_casa(
            OPTION_ENUM
            + "Option::None match\n"
            + '    Option::Some(value) => "some" print\n'
            + '    Option::None => "none" print\n'
            + "end\n"
        )
        assert output == "none"

    def test_result_ok_destructure(self, run_casa):
        output = run_casa(
            RESULT_ENUM
            + "5 Result::Ok match\n"
            + "    Result::Error(err) => err print\n"
            + "    Result::Ok(val) => val print\n"
            + "end\n"
        )
        assert output == "5"

    def test_result_error_destructure(self, run_casa):
        output = run_casa(
            RESULT_ENUM
            + '"oops" Result::Error match\n'
            + "    Result::Error(err) => err print\n"
            + '    Result::Ok(val) => "ok" print\n'
            + "end\n"
        )
        assert output == "oops"

    def test_data_enum_wildcard(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "Shape::Point match\n"
            + "    Shape::Circle(radius) => radius print\n"
            + '    _ => "other" print\n'
            + "end\n"
        )
        assert output == "other"

    def test_data_enum_eq(self, run_casa):
        output = run_casa(SHAPE_ENUM + "Shape::Point Shape::Point == print\n")
        assert output == "true"

    def test_data_enum_ne(self, run_casa):
        output = run_casa(SHAPE_ENUM + "10 Shape::Circle Shape::Point != print\n")
        assert output == "true"

    def test_enum_lt_true(self, run_casa):
        # RPN: Color::Blue Color::Red < means Red < Blue (0 < 2)
        output = run_casa(COLOR_ENUM + "Color::Blue Color::Red < print\n")
        assert output == "true"

    def test_enum_lt_false(self, run_casa):
        output = run_casa(COLOR_ENUM + "Color::Red Color::Blue < print\n")
        assert output == "false"

    def test_enum_le_true(self, run_casa):
        output = run_casa(COLOR_ENUM + "Color::Red Color::Red <= print\n")
        assert output == "true"

    def test_enum_gt_true(self, run_casa):
        # RPN: Color::Red Color::Blue > means Blue > Red (2 > 0)
        output = run_casa(COLOR_ENUM + "Color::Red Color::Blue > print\n")
        assert output == "true"

    def test_enum_ge_true(self, run_casa):
        output = run_casa(COLOR_ENUM + "Color::Green Color::Green >= print\n")
        assert output == "true"

    def test_data_enum_lt(self, run_casa):
        # RPN: Shape::Point 10 Shape::Circle < means Circle < Point (0 < 2)
        output = run_casa(SHAPE_ENUM + "Shape::Point 10 Shape::Circle < print\n")
        assert output == "true"


# ---------------------------------------------------------------------------
# Literal pattern matching — parsing
# ---------------------------------------------------------------------------
class TestLiteralMatchParsing:
    def test_bool_literal_parsed_as_literal_pattern(self):
        ops = parse_string("true match\n" "    true => 1\n" "    false => 0\n" "end\n")
        arms = [op for op in ops if op.kind == OpKind.MATCH_ARM]
        assert len(arms) == 2
        assert isinstance(arms[0].value, LiteralPattern)
        assert arms[0].value.typ == "bool"
        assert arms[0].value.value is True
        assert isinstance(arms[1].value, LiteralPattern)
        assert arms[1].value.value is False

    def test_int_literal_parsed_as_literal_pattern(self):
        ops = parse_string("42 match\n" "    0 => 1\n" "    _ => 2\n" "end\n")
        arms = [op for op in ops if op.kind == OpKind.MATCH_ARM]
        assert len(arms) == 2
        assert isinstance(arms[0].value, LiteralPattern)
        assert arms[0].value.typ == "int"
        assert arms[0].value.value == 0

    def test_char_literal_parsed_as_literal_pattern(self):
        ops = parse_string("'a' match\n" "    'a' => 1\n" "    _ => 2\n" "end\n")
        arms = [op for op in ops if op.kind == OpKind.MATCH_ARM]
        assert isinstance(arms[0].value, LiteralPattern)
        assert arms[0].value.typ == "char"
        assert arms[0].value.value == ord("a")

    def test_str_literal_parsed_as_literal_pattern(self):
        ops = parse_string(
            '"hello" match\n' '    "hello" => 1\n' "    _ => 2\n" "end\n"
        )
        arms = [op for op in ops if op.kind == OpKind.MATCH_ARM]
        assert isinstance(arms[0].value, LiteralPattern)
        assert arms[0].value.typ == "str"
        assert arms[0].value.value == "hello"

    def test_negative_int_literal_parsed(self):
        ops = parse_string("42 match\n" "    -1 => 1\n" "    _ => 2\n" "end\n")
        arms = [op for op in ops if op.kind == OpKind.MATCH_ARM]
        assert isinstance(arms[0].value, LiteralPattern)
        assert arms[0].value.value == -1


# ---------------------------------------------------------------------------
# Literal pattern matching — type checking
# ---------------------------------------------------------------------------
class TestLiteralMatchTypecheck:
    def test_bool_exhaustive_passes(self):
        sig = typecheck_string(
            "true match\n" "    true => 1\n" "    false => 0\n" "end\n"
        )
        assert sig.return_types == ["int"]

    def test_bool_with_wildcard_passes(self):
        sig = typecheck_string("true match\n" "    true => 1\n" "    _ => 0\n" "end\n")
        assert sig.return_types == ["int"]

    def test_bool_missing_arm_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("true match\n" "    true => 1\n" "end\n")
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH
        assert "false" in error.message

    def test_int_with_wildcard_passes(self):
        sig = typecheck_string(
            "42 match\n" "    0 => 1\n" "    1 => 2\n" "    _ => 3\n" "end\n"
        )
        assert sig.return_types == ["int"]

    def test_int_without_wildcard_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("42 match\n" "    0 => 1\n" "    1 => 2\n" "end\n")
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH
        assert "wildcard" in error.message

    def test_char_with_wildcard_passes(self):
        sig = typecheck_string("'a' match\n" "    'a' => 1\n" "    _ => 0\n" "end\n")
        assert sig.return_types == ["int"]

    def test_char_without_wildcard_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("'a' match\n" "    'a' => 1\n" "end\n")
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH

    def test_str_with_wildcard_passes(self):
        sig = typecheck_string(
            '"hello" match\n' '    "hello" => 1\n' "    _ => 0\n" "end\n"
        )
        assert sig.return_types == ["int"]

    def test_str_without_wildcard_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string('"hello" match\n' '    "hello" => 1\n' "end\n")
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH

    def test_type_mismatch_int_literal_in_bool_match(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("true match\n" "    42 => 1\n" "    _ => 0\n" "end\n")
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH

    def test_type_mismatch_bool_literal_in_int_match(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("42 match\n" "    true => 1\n" "    _ => 0\n" "end\n")
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH

    def test_duplicate_literal_arm_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                "42 match\n" "    0 => 1\n" "    0 => 2\n" "    _ => 3\n" "end\n"
            )
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.DUPLICATE_NAME

    def test_literal_arms_after_wildcard_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("42 match\n" "    _ => 1\n" "    0 => 2\n" "end\n")
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.SYNTAX

    def test_enum_variant_in_int_match_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM + "42 match\n" "    Color::Red => 1\n" "    _ => 2\n" "end\n"
            )
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH

    def test_literal_in_enum_match_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM + "Color::Red match\n" "    42 => 1\n" "    _ => 2\n" "end\n"
            )
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH

    def test_bare_identifier_in_literal_match_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string("42 match\n" "    foo => 1\n" "    _ => 2\n" "end\n")
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH


# ---------------------------------------------------------------------------
# Literal pattern matching — bytecode
# ---------------------------------------------------------------------------
class TestLiteralMatchBytecode:
    def test_int_match_compiles_dup_push_eq_pattern(self):
        program = compile_string("42 match\n" "    0 => 1\n" "    _ => 2\n" "end\n")
        kinds = [i.kind for i in program.bytecode]
        assert InstKind.DUP in kinds
        assert InstKind.EQ in kinds

    def test_bool_match_compiles(self):
        program = compile_string(
            "true match\n" "    true => 1\n" "    false => 0\n" "end\n"
        )
        kinds = [i.kind for i in program.bytecode]
        assert InstKind.DUP in kinds
        assert InstKind.EQ in kinds

    def test_str_match_compiles_str_eq(self):
        program = compile_string(
            '"hello" match\n' '    "hello" => 1\n' "    _ => 2\n" "end\n"
        )
        kinds = [i.kind for i in program.bytecode]
        assert InstKind.STR_EQ in kinds

    def test_str_eq_compiles_str_eq_inst(self):
        program = compile_string('"hello" "world" ==\n')
        kinds = [i.kind for i in program.bytecode]
        assert InstKind.STR_EQ in kinds

    def test_str_ne_compiles_str_eq_and_not(self):
        program = compile_string('"hello" "world" !=\n')
        kinds = [i.kind for i in program.bytecode]
        assert InstKind.STR_EQ in kinds
        assert InstKind.NOT in kinds


# ---------------------------------------------------------------------------
# Literal pattern matching — end-to-end
# ---------------------------------------------------------------------------
class TestLiteralMatchEndToEnd:
    @pytest.fixture
    def run_casa(self, tmp_path):
        def _run(code: str) -> str:
            src = tmp_path / "test.casa"
            src.write_text(code)
            binary = tmp_path / "test"
            result = subprocess.run(
                ["python3", "casa.py", str(src), "-o", str(binary)],
                capture_output=True,
                text=True,
                cwd=CASA_ROOT,
                timeout=30,
            )
            if result.returncode != 0:
                pytest.fail(
                    f"Compilation failed:\nstdout: {result.stdout}\nstderr: {result.stderr}"
                )
            run_result = subprocess.run(
                [str(binary)],
                capture_output=True,
                text=True,
                timeout=10,
            )
            return run_result.stdout

        return _run

    def test_bool_match(self, run_casa):
        output = run_casa(
            "fn check flag:bool {\n"
            "    flag match\n"
            '        true => "yes" print\n'
            '        false => "no" print\n'
            "    end\n"
            "}\n"
            "true check\n"
            "false check\n"
        )
        assert output == "yesno"

    def test_int_match(self, run_casa):
        output = run_casa(
            "fn describe n:int {\n"
            "    n match\n"
            '        0 => "zero" print\n'
            '        1 => "one" print\n'
            '        _ => "other" print\n'
            "    end\n"
            "}\n"
            "0 describe\n"
            "1 describe\n"
            "99 describe\n"
        )
        assert output == "zerooneother"

    def test_char_match(self, run_casa):
        output = run_casa(
            "fn check ch:char {\n"
            "    ch match\n"
            "        'y' => \"yes\" print\n"
            "        'n' => \"no\" print\n"
            '        _ => "unknown" print\n'
            "    end\n"
            "}\n"
            "'y' check\n"
            "'n' check\n"
            "'x' check\n"
        )
        assert output == "yesnounknown"

    def test_str_match(self, run_casa):
        output = run_casa(
            "fn greet name:str {\n"
            "    name match\n"
            '        "Alice" => "Hi Alice" print\n'
            '        "Bob" => "Hi Bob" print\n'
            '        _ => "Hi stranger" print\n'
            "    end\n"
            "}\n"
            '"Alice" greet\n'
            '"Bob" greet\n'
            '"Charlie" greet\n'
        )
        assert output == "Hi AliceHi BobHi stranger"

    def test_int_match_negative(self, run_casa):
        output = run_casa(
            "fn check n:int {\n"
            "    n match\n"
            '        -1 => "neg" print\n'
            '        0 => "zero" print\n'
            '        _ => "pos" print\n'
            "    end\n"
            "}\n"
            "-1 check\n"
            "0 check\n"
            "5 check\n"
        )
        assert output == "negzeropos"

    def test_bool_match_wildcard_only(self, run_casa):
        output = run_casa("true match\n" '    _ => "matched" print\n' "end\n")
        assert output == "matched"

    def test_int_match_single_arm_with_wildcard(self, run_casa):
        output = run_casa("42 match\n" '    _ => "any" print\n' "end\n")
        assert output == "any"


# ---------------------------------------------------------------------------
# String equality comparison — end-to-end
# ---------------------------------------------------------------------------
class TestStrComparisonEndToEnd:
    @pytest.fixture
    def run_casa(self, tmp_path):
        def _run(code: str) -> str:
            src = tmp_path / "test.casa"
            src.write_text(code)
            binary = tmp_path / "test"
            result = subprocess.run(
                ["python3", "casa.py", str(src), "-o", str(binary)],
                capture_output=True,
                text=True,
                cwd=CASA_ROOT,
                timeout=30,
            )
            if result.returncode != 0:
                pytest.fail(
                    f"Compilation failed:\nstdout: {result.stdout}\nstderr: {result.stderr}"
                )
            run_result = subprocess.run(
                [str(binary)],
                capture_output=True,
                text=True,
                timeout=10,
            )
            return run_result.stdout

        return _run

    def test_str_eq_same_literal(self, run_casa):
        output = run_casa('"hello" "hello" == print\n')
        assert output == "true"

    def test_str_eq_different_literal(self, run_casa):
        output = run_casa('"hello" "world" == print\n')
        assert output == "false"

    def test_str_ne_different(self, run_casa):
        output = run_casa('"hello" "world" != print\n')
        assert output == "true"

    def test_str_ne_same(self, run_casa):
        output = run_casa('"hello" "hello" != print\n')
        assert output == "false"

    def test_str_eq_empty_strings(self, run_casa):
        output = run_casa('"" "" == print\n')
        assert output == "true"

    def test_str_eq_empty_vs_nonempty(self, run_casa):
        output = run_casa('"" "a" == print\n')
        assert output == "false"

    def test_str_eq_in_conditional(self, run_casa):
        output = run_casa(
            '"Alice" = name\n' 'if name "Alice" == then\n' '    "yes" print\n' "fi\n"
        )
        assert output == "yes"

    def test_str_eq_variable_comparison(self, run_casa):
        output = run_casa(
            "fn check a:str b:str -> bool {\n"
            "    a b ==\n"
            "}\n"
            '"hello" "hello" check print\n'
            '"hello" "world" check print\n'
        )
        assert output == "truefalse"


# ---------------------------------------------------------------------------
# `is` keyword - parsing
# ---------------------------------------------------------------------------
class TestIsCheckParsing:
    def test_plain_enum_is_check(self):
        ops = resolve_string(COLOR_ENUM + "Color::Red = c\nc Color::Red is\n")
        is_ops = find_ops(ops, OpKind.IS_CHECK)
        assert len(is_ops) == 1
        variant = is_ops[0].value
        assert isinstance(variant, EnumVariant)
        assert variant.enum_name == "Color"
        assert variant.variant_name == "Red"
        assert variant.bindings == []

    def test_is_check_with_bindings(self):
        ops = resolve_string(
            SHAPE_ENUM
            + "10 Shape::Circle = s\n"
            + "if s Shape::Circle(r) is then\nr print\nfi\n"
        )
        is_ops = find_ops(ops, OpKind.IS_CHECK)
        assert len(is_ops) == 1
        variant = is_ops[0].value
        assert variant.bindings == ["r"]

    def test_is_check_multiple_bindings(self):
        ops = resolve_string(
            SHAPE_ENUM
            + "3 4 Shape::Rectangle = s\n"
            + "if s Shape::Rectangle(w h) is then\nw print\nfi\n"
        )
        is_ops = find_ops(ops, OpKind.IS_CHECK)
        assert len(is_ops) == 1
        assert is_ops[0].value.bindings == ["w", "h"]

    def test_is_check_resolves_ordinal(self):
        ops = resolve_string(COLOR_ENUM + "Color::Blue = c\nc Color::Blue is\n")
        is_ops = find_ops(ops, OpKind.IS_CHECK)
        assert is_ops[0].value.ordinal == 2

    def test_bare_is_keyword_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            resolve_string(COLOR_ENUM + "Color::Red = c\nc is\n")
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX


# ---------------------------------------------------------------------------
# `is` keyword - type checking
# ---------------------------------------------------------------------------
class TestIsCheckTypeChecking:
    def test_is_pushes_bool(self):
        sig = typecheck_string(COLOR_ENUM + "Color::Red = c\nc Color::Red is\n")
        assert sig.return_types == ["bool"]

    def test_is_type_mismatch(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM + DIRECTION_ENUM + "Color::Red = c\nc Direction::North is\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_is_non_enum_type(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(COLOR_ENUM + "42 = n\nn Color::Red is\n")
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_is_bindings_outside_if_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                SHAPE_ENUM + "10 Shape::Circle = s\ns Shape::Circle(r) is\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_is_wrong_binding_count(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                SHAPE_ENUM
                + "10 Shape::Circle = s\n"
                + "if s Shape::Circle(a b) is then\na print\nfi\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_is_bindings_on_plain_variant(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + "Color::Red = c\n"
                + "if c Color::Red(x) is then\nx print\nfi\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.TYPE_MISMATCH

    def test_is_bindings_in_while_condition_error(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                SHAPE_ENUM
                + "10 Shape::Circle = s\n"
                + "while s Shape::Circle(r) is do\nr print\ndone\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX

    def test_is_generic_enum(self):
        sig = typecheck_string(OPTION_ENUM + "42 Option::Some = m\nm Option::Some is\n")
        assert sig.return_types == ["bool"]

    def test_is_generic_with_bindings(self):
        typecheck_string(
            OPTION_ENUM
            + "42 Option::Some = m\n"
            + "if m Option::Some(v) is then\nv print\nfi\n"
        )


# ---------------------------------------------------------------------------
# `is` keyword - bytecode
# ---------------------------------------------------------------------------
class TestIsCheckBytecode:
    def test_plain_enum_bytecode(self):
        program = compile_string(COLOR_ENUM + "Color::Red = c\nc Color::Red is\n")
        inst_kinds = [i.kind for i in program.bytecode]
        # Should contain PUSH (ordinal) + EQ for plain enum
        assert InstKind.EQ in inst_kinds

    def test_data_carrying_no_bindings_bytecode(self):
        program = compile_string(
            SHAPE_ENUM + "10 Shape::Circle = s\ns Shape::Circle is\n"
        )
        inst_kinds = [i.kind for i in program.bytecode]
        # Should contain LOAD64 (read ordinal from heap) + PUSH + EQ
        assert InstKind.LOAD64 in inst_kinds
        assert InstKind.EQ in inst_kinds


# ---------------------------------------------------------------------------
# `is` keyword - end to end
# ---------------------------------------------------------------------------
class TestIsCheckEndToEnd:
    @pytest.fixture
    def run_casa(self, tmp_path):
        def _run(code: str) -> str:
            src = tmp_path / "test.casa"
            src.write_text(code)
            binary = tmp_path / "test"
            result = subprocess.run(
                ["python3", "casa.py", str(src), "-o", str(binary)],
                capture_output=True,
                text=True,
                cwd=CASA_ROOT,
                timeout=30,
            )
            if result.returncode != 0:
                pytest.fail(
                    f"Compilation failed:\nstdout: {result.stdout}\nstderr: {result.stderr}"
                )
            run_result = subprocess.run(
                [str(binary)],
                capture_output=True,
                text=True,
                timeout=10,
            )
            return run_result.stdout

        return _run

    def test_plain_enum_is_true(self, run_casa):
        output = run_casa(COLOR_ENUM + "Color::Red = c\nc Color::Red is print\n")
        assert output == "true"

    def test_plain_enum_is_false(self, run_casa):
        output = run_casa(COLOR_ENUM + "Color::Red = c\nc Color::Blue is print\n")
        assert output == "false"

    def test_data_carrying_is_true(self, run_casa):
        output = run_casa(
            SHAPE_ENUM + "10 Shape::Circle = s\ns Shape::Circle is print\n"
        )
        assert output == "true"

    def test_data_carrying_is_false(self, run_casa):
        output = run_casa(
            SHAPE_ENUM + "10 Shape::Circle = s\ns Shape::Rectangle is print\n"
        )
        assert output == "false"

    def test_is_with_bindings_if(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "10 Shape::Circle = s\n"
            + "if s Shape::Circle(r) is then\n"
            + "    r print\n"
            + "fi\n"
        )
        assert output == "10"

    def test_is_with_bindings_elif(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "3 4 Shape::Rectangle = s\n"
            + "if s Shape::Circle(r) is then\n"
            + '    "circle" print\n'
            + "elif s Shape::Rectangle(w h) is then\n"
            + "    w h * print\n"
            + "fi\n"
        )
        assert output == "12"

    def test_is_with_bindings_no_match(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "Shape::Point = s\n"
            + "if s Shape::Circle(r) is then\n"
            + '    "circle" print\n'
            + "else\n"
            + '    "other" print\n'
            + "fi\n"
        )
        assert output == "other"

    def test_is_generic_enum(self, run_casa):
        output = run_casa(
            OPTION_ENUM
            + "42 Option::Some = m\n"
            + "if m Option::Some(v) is then\n"
            + "    v print\n"
            + "fi\n"
        )
        assert output == "42"

    def test_is_generic_none(self, run_casa):
        output = run_casa(
            OPTION_ENUM + "Option::None = m\n" + "m Option::Some is print\n"
        )
        assert output == "false"

    def test_is_in_function(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "fn get_radius s:Shape -> int {\n"
            + "    if s Shape::Circle(r) is then\n"
            + "        r return\n"
            + "    fi\n"
            + "    0\n"
            + "}\n"
            + "10 Shape::Circle get_radius print\n"
            + "Shape::Point get_radius print\n"
        )
        assert output == "100"

    def test_is_multiple_bindings(self, run_casa):
        output = run_casa(
            SHAPE_ENUM
            + "3 4 Shape::Rectangle = s\n"
            + "if s Shape::Rectangle(w h) is then\n"
            + '    w print " " print h print\n'
            + "fi\n"
        )
        assert output == "3 4"

    def test_is_no_data_variant_on_data_enum_true(self, run_casa):
        output = run_casa(SHAPE_ENUM + "Shape::Point = p\np Shape::Point is print\n")
        assert output == "true"

    def test_is_no_data_variant_on_data_enum_false(self, run_casa):
        output = run_casa(
            SHAPE_ENUM + "10 Shape::Circle = s\ns Shape::Point is print\n"
        )
        assert output == "false"

    def test_is_without_bindings_in_function(self, run_casa):
        output = run_casa(
            OPTION_ENUM
            + "fn is_some o:Option[int] -> bool { o Option::Some is }\n"
            + "42 Option::Some is_some print\n"
            + "Option::None is_some print\n"
        )
        assert output == "truefalse"

    def test_is_plain_enum_without_bindings_in_while(self, run_casa):
        output = run_casa(COLOR_ENUM + "Color::Red = c\n" + "c Color::Red is print\n")
        assert output == "true"
