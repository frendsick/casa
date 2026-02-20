"""Tests for casa/error.py -- error formatting and offset_to_line_col."""

import pytest
from pathlib import Path

from casa.common import Location, Span
from casa.error import CasaError, CasaErrorCollection, ErrorKind, offset_to_line_col
from tests.conftest import resolve_string


# ---------------------------------------------------------------------------
# offset_to_line_col
# ---------------------------------------------------------------------------
def test_offset_to_line_col_first_line():
    source = "hello world"
    line, col, source_line = offset_to_line_col(source, 0)
    assert line == 1
    assert col == 1
    assert source_line == "hello world"


def test_offset_to_line_col_middle_of_first_line():
    source = "hello world"
    line, col, source_line = offset_to_line_col(source, 6)
    assert line == 1
    assert col == 7
    assert source_line == "hello world"


def test_offset_to_line_col_second_line():
    source = "line1\nline2\nline3"
    line, col, source_line = offset_to_line_col(source, 6)
    assert line == 2
    assert col == 1
    assert source_line == "line2"


def test_offset_to_line_col_third_line_offset():
    source = "abc\ndef\nghi"
    line, col, source_line = offset_to_line_col(source, 9)
    assert line == 3
    assert col == 2
    assert source_line == "ghi"


def test_offset_to_line_col_end_of_source():
    source = "abc\ndef"
    line, col, source_line = offset_to_line_col(source, 7)
    assert line == 2
    assert col == 4
    assert source_line == "def"


# ---------------------------------------------------------------------------
# CasaError.format
# ---------------------------------------------------------------------------
def test_casa_error_format_no_location():
    err = CasaError(ErrorKind.SYNTAX, "bad token")
    result = err.format({})
    assert result == "error[SYNTAX]: bad token"


def test_casa_error_format_with_location_no_source():
    loc = Location(Path("test.casa"), Span(0, 3))
    err = CasaError(ErrorKind.UNDEFINED_NAME, "not found", loc)
    result = err.format({})
    assert "test.casa" in result


def test_casa_error_format_with_source():
    source = "foo bar baz"
    cache = {Path("test.casa"): source}
    loc = Location(Path("test.casa"), Span(4, 3))
    err = CasaError(ErrorKind.UNDEFINED_NAME, "`bar` is not defined", loc)
    result = err.format(cache)
    assert "error[UNDEFINED_NAME]" in result
    assert "test.casa:1:5" in result
    assert "foo bar baz" in result
    assert "^^^" in result


def test_casa_error_format_multiline_source():
    source = "10 = x\nfoo\n20 = y"
    cache = {Path("test.casa"): source}
    loc = Location(Path("test.casa"), Span(7, 3))
    err = CasaError(ErrorKind.UNDEFINED_NAME, "`foo` is not defined", loc)
    result = err.format(cache)
    assert "test.casa:2:1" in result
    assert "foo" in result
    assert "^^^" in result


def test_casa_error_format_with_note():
    source = '"hello" 42 +'
    cache = {Path("test.casa"): source}
    err = CasaError(
        ErrorKind.TYPE_MISMATCH,
        "Type mismatch",
        Location(Path("test.casa"), Span(8, 2)),
        expected="`int`",
        got=("Got", "`str`"),
        note=(
            "value of type `str` pushed here",
            Location(Path("test.casa"), Span(0, 7)),
        ),
    )
    result = err.format(cache)
    assert "note: value of type `str` pushed here" in result
    assert "test.casa:1:1" in result
    assert '"hello"' in result
    assert "^^^^^^^" in result


def test_casa_error_format_multiline_span():
    source = '"hello\nworld'
    cache = {Path("test.casa"): source}
    loc = Location(Path("test.casa"), Span(0, 12))
    err = CasaError(ErrorKind.SYNTAX, "Unclosed string literal", loc)
    result = err.format(cache)
    assert "test.casa:1:1" in result
    assert '"hello' in result
    assert "world" in result
    lines = result.split("\n")
    caret_lines = [l for l in lines if "^" in l]
    assert len(caret_lines) == 2


def test_casa_error_format_multiline_span_three_lines():
    source = '"hello\nfoo\nworld'
    cache = {Path("test.casa"): source}
    loc = Location(Path("test.casa"), Span(0, 16))
    err = CasaError(ErrorKind.SYNTAX, "Unclosed string literal", loc)
    result = err.format(cache)
    assert "test.casa:1:1" in result
    lines = result.split("\n")
    caret_lines = [l for l in lines if "^" in l]
    assert len(caret_lines) == 3


# ---------------------------------------------------------------------------
# CasaErrorCollection
# ---------------------------------------------------------------------------
def test_casa_error_collection_single():
    err = CasaError(ErrorKind.SYNTAX, "oops")
    collection = CasaErrorCollection(err)
    assert len(collection.errors) == 1
    assert "1 error(s)" in str(collection)


def test_casa_error_collection_multiple():
    errors = [
        CasaError(ErrorKind.SYNTAX, "first"),
        CasaError(ErrorKind.UNDEFINED_NAME, "second"),
    ]
    collection = CasaErrorCollection(errors)
    assert len(collection.errors) == 2
    assert "2 error(s)" in str(collection)


# ---------------------------------------------------------------------------
# Multi-error integration tests
# ---------------------------------------------------------------------------
def test_multi_error_undefined_names():
    """Multiple undefined identifiers should be collected into one exception."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        resolve_string("foo bar baz")
    errors = exc_info.value.errors
    assert len(errors) == 3
    assert all(e.kind == ErrorKind.UNDEFINED_NAME for e in errors)
    names = [e.message for e in errors]
    assert any("foo" in m for m in names)
    assert any("bar" in m for m in names)
    assert any("baz" in m for m in names)


def test_multi_error_preserves_location():
    """Each collected error should have a location with correct offset."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        resolve_string("aaa bbb")
    errors = exc_info.value.errors
    assert len(errors) == 2
    assert errors[0].location is not None
    assert errors[1].location is not None
    assert errors[0].location.span.offset < errors[1].location.span.offset
