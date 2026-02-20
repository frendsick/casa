"""Tests for casa/lexer.py — covers all TokenKind, Keyword, Intrinsic, Operator, Delimiter values."""

import pytest

from casa.common import Delimiter, Intrinsic, Keyword, Operator, TokenKind
from casa.error import CasaErrorCollection, ErrorKind
from tests.conftest import lex_string


# ---------------------------------------------------------------------------
# Keywords
# ---------------------------------------------------------------------------
ALL_KEYWORDS = [
    "fn",
    "impl",
    "return",
    "while",
    "do",
    "break",
    "continue",
    "done",
    "if",
    "then",
    "elif",
    "else",
    "fi",
    "struct",
    "include",
]
assert len(ALL_KEYWORDS) == len(Keyword)


@pytest.mark.parametrize("word", ALL_KEYWORDS)
def test_lex_all_keywords(word):
    tokens = lex_string(word)
    # Last token is EOF
    assert tokens[0].kind == TokenKind.KEYWORD
    assert tokens[0].value == word


# ---------------------------------------------------------------------------
# Intrinsics
# ---------------------------------------------------------------------------
ALL_INTRINSICS = [
    "drop",
    "dup",
    "over",
    "rot",
    "swap",
    "alloc",
    "load",
    "store",
    "print",
    "exec",
]
assert len(ALL_INTRINSICS) == len(Intrinsic)


@pytest.mark.parametrize("word", ALL_INTRINSICS)
def test_lex_all_intrinsics(word):
    tokens = lex_string(word)
    assert tokens[0].kind == TokenKind.INTRINSIC
    assert tokens[0].value == word


# ---------------------------------------------------------------------------
# Operators
# ---------------------------------------------------------------------------
ALL_OPERATORS = [
    "+",
    "-",
    "*",
    "/",
    "%",
    "<<",
    ">>",
    "&&",
    "||",
    "!",
    "==",
    ">=",
    ">",
    "<=",
    "<",
    "!=",
    "=",
    "-=",
    "+=",
]
assert len(ALL_OPERATORS) == len(Operator)


@pytest.mark.parametrize("op", ALL_OPERATORS)
def test_lex_all_operators(op):
    # Operators like = need an identifier after them to not cause parse issues,
    # but the lexer itself should still produce an OPERATOR token.
    tokens = lex_string(op)
    assert tokens[0].kind == TokenKind.OPERATOR
    assert tokens[0].value == op


# ---------------------------------------------------------------------------
# Delimiters
# ---------------------------------------------------------------------------
# # (HASHTAG) is a comment — the lexer skips it, so we test it separately.
DELIMITER_PAIRS = [
    ("->", "->"),
    (",", ","),
    (":", ":"),
    (".", "."),
    ("{", "{"),
    ("}", "}"),
    ("[", "["),
    ("]", "]"),
    ("(", "("),
    (")", ")"),
]


@pytest.mark.parametrize("text,expected_value", DELIMITER_PAIRS)
def test_lex_all_delimiters(text, expected_value):
    tokens = lex_string(text)
    assert tokens[0].kind == TokenKind.DELIMITER
    assert tokens[0].value == expected_value


def test_lex_hashtag_comment_skips():
    """HASHTAG delimiter causes the rest of the line to be skipped."""
    tokens = lex_string("# this is a comment")
    assert len(tokens) == 1
    assert tokens[0].kind == TokenKind.EOF


# ---------------------------------------------------------------------------
# Literals
# ---------------------------------------------------------------------------
def test_lex_integer_literal():
    tokens = lex_string("42")
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == "42"


@pytest.mark.parametrize("word", ["true", "false"])
def test_lex_boolean_literals(word):
    tokens = lex_string(word)
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == word


def test_lex_negative_integer_literal():
    tokens = lex_string("-42")
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == "-42"


def test_lex_minus_operator_with_space():
    tokens = lex_string("- 42")
    assert tokens[0].kind == TokenKind.OPERATOR
    assert tokens[0].value == "-"
    assert tokens[1].kind == TokenKind.LITERAL
    assert tokens[1].value == "42"


def test_lex_negative_after_int_no_space():
    """10-3 is not a negative literal — the minus is stuck to the previous token."""
    tokens = lex_string("10-3")
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == "10"
    # -3 falls through to IDENTIFIER (not LITERAL), will error at resolution
    assert tokens[1].kind == TokenKind.IDENTIFIER
    assert tokens[1].value == "-3"


def test_lex_string_literal():
    tokens = lex_string('"hello"')
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == '"hello"'


def test_lex_empty_string_literal():
    tokens = lex_string('""')
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == '""'


# ---------------------------------------------------------------------------
# Identifiers
# ---------------------------------------------------------------------------
def test_lex_identifier():
    tokens = lex_string("my_var")
    assert tokens[0].kind == TokenKind.IDENTIFIER
    assert tokens[0].value == "my_var"


def test_lex_identifier_capitalized():
    tokens = lex_string("Person")
    assert tokens[0].kind == TokenKind.IDENTIFIER
    assert tokens[0].value == "Person"


def test_lex_identifier_namespaced():
    tokens = lex_string("A::b")
    assert tokens[0].kind == TokenKind.IDENTIFIER
    assert tokens[0].value == "A::b"


# ---------------------------------------------------------------------------
# EOF
# ---------------------------------------------------------------------------
def test_lex_eof():
    tokens = lex_string("")
    assert len(tokens) == 1
    assert tokens[0].kind == TokenKind.EOF


# ---------------------------------------------------------------------------
# Comments
# ---------------------------------------------------------------------------
def test_lex_comment_skipping():
    tokens = lex_string("42 # comment\n43")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 2
    assert non_eof[0].value == "42"
    assert non_eof[1].value == "43"


# ---------------------------------------------------------------------------
# Location tracking
# ---------------------------------------------------------------------------
def test_lex_location_tracking():
    tokens = lex_string("42 hello")
    assert tokens[0].location.span.offset == 0
    assert tokens[0].location.span.length == 2
    assert tokens[1].location.span.offset == 3
    assert tokens[1].location.span.length == 5


# ---------------------------------------------------------------------------
# Full expression
# ---------------------------------------------------------------------------
def test_lex_full_expression():
    tokens = lex_string("34 35 + print")
    kinds = [t.kind for t in tokens]
    assert kinds == [
        TokenKind.LITERAL,
        TokenKind.LITERAL,
        TokenKind.OPERATOR,
        TokenKind.INTRINSIC,
        TokenKind.EOF,
    ]
    values = [t.value for t in tokens[:-1]]
    assert values == ["34", "35", "+", "print"]


# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------
def test_lex_unclosed_string_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string('"hello')
    assert len(exc_info.value.errors) == 1
    assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX
    assert "Unclosed string" in exc_info.value.errors[0].message


def test_lex_missing_method_name_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string("Foo::")
    assert len(exc_info.value.errors) == 1
    assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX
    assert "method name" in exc_info.value.errors[0].message
