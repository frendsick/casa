"""Tests for unqualified enum variant hints in match arms.

These tests verify that the type checker (not the parser) detects bare
variant names in match arms and provides helpful error hints.
"""

import pytest

from casa.error import CasaErrorCollection, ErrorKind
from tests.conftest import typecheck_string

COLOR_ENUM = "enum Color { Red Green Blue }\n"
DIRECTION_ENUM = "enum Direction { North South East West }\n"


class TestBareVariantHintFromTypeChecker:
    """Bare variant names in match arms should produce type checker errors with hints."""

    def test_bare_variant_produces_type_mismatch(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + "Color::Green match\n"
                + "    Color::Red => 1\n"
                + "    Green => 2\n"
                + "    Color::Blue => 3\n"
                + "end\n"
            )
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH

    def test_bare_variant_hint_contains_qualified_name(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + "Color::Green match\n"
                + "    Color::Red => 1\n"
                + "    Green => 2\n"
                + "    Color::Blue => 3\n"
                + "end\n"
            )
        error = exc_info.value.errors[0]
        assert "Did you mean `Color::Green`?" in error.message

    def test_bare_variant_first_arm(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + "Color::Red match\n"
                + "    Red => 1\n"
                + "    Color::Green => 2\n"
                + "    Color::Blue => 3\n"
                + "end\n"
            )
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH
        assert "Did you mean `Color::Red`?" in error.message

    def test_bare_variant_last_arm(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + "Color::Red match\n"
                + "    Color::Red => 1\n"
                + "    Color::Green => 2\n"
                + "    Blue => 3\n"
                + "end\n"
            )
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH
        assert "Did you mean `Color::Blue`?" in error.message

    def test_bare_variant_not_in_any_enum_no_hint(self):
        """A bare name that is not a variant of the matched enum gets no hint."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + "Color::Red match\n"
                + "    Color::Red => 1\n"
                + "    Foo => 2\n"
                + "    Color::Blue => 3\n"
                + "end\n"
            )
        error = exc_info.value.errors[0]
        assert "Did you mean" not in error.message

    def test_all_bare_variants_errors_on_first(self):
        """When all arms use bare variants, the error fires on the first one."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + "Color::Red match\n"
                + "    Red => 1\n"
                + "    Green => 2\n"
                + "    Blue => 3\n"
                + "end\n"
            )
        error = exc_info.value.errors[0]
        assert error.kind == ErrorKind.TYPE_MISMATCH
        assert "Did you mean `Color::Red`?" in error.message

    def test_bare_variant_from_different_enum_no_hint(self):
        """A bare name that is a variant of a different enum gets no hint."""
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(
                COLOR_ENUM
                + DIRECTION_ENUM
                + "Color::Red match\n"
                + "    Color::Red => 1\n"
                + "    North => 2\n"
                + "    Color::Blue => 3\n"
                + "end\n"
            )
        error = exc_info.value.errors[0]
        assert "Did you mean" not in error.message


class TestQualifiedVariantsStillWork:
    """Qualified variants should continue to work correctly after the refactoring."""

    def test_qualified_match_all_arms(self):
        typecheck_string(
            COLOR_ENUM
            + "Color::Red match\n"
            + "    Color::Red => 1\n"
            + "    Color::Green => 2\n"
            + "    Color::Blue => 3\n"
            + "end\n"
            + "drop\n"
        )

    def test_qualified_match_with_function(self):
        typecheck_string(
            COLOR_ENUM
            + "fn color_val c:Color -> int {\n"
            + "    c match\n"
            + "        Color::Red => 0\n"
            + "        Color::Green => 1\n"
            + "        Color::Blue => 2\n"
            + "    end\n"
            + "}\n"
            + "Color::Green color_val\n"
            + "drop\n"
        )


class TestWildcardStillWorks:
    """Wildcard match arms should continue to work correctly."""

    def test_wildcard_covers_remaining(self):
        typecheck_string(
            COLOR_ENUM
            + "Color::Red match\n"
            + "    Color::Red => 1\n"
            + "    _ => 0\n"
            + "end\n"
            + "drop\n"
        )

    def test_wildcard_with_one_explicit_arm(self):
        typecheck_string(
            COLOR_ENUM
            + "Color::Green match\n"
            + "    Color::Red => 1\n"
            + "    _ => 0\n"
            + "end\n"
            + "drop\n"
        )

    def test_wildcard_with_most_arms(self):
        typecheck_string(
            COLOR_ENUM
            + "Color::Red match\n"
            + "    Color::Red => 1\n"
            + "    Color::Green => 2\n"
            + "    _ => 0\n"
            + "end\n"
            + "drop\n"
        )
