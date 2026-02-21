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
    err = exc_info.value.errors[0]
    assert err.kind == ErrorKind.SYNTAX
    assert "Unclosed string" in err.message
    assert err.location.span.offset == 0
    assert err.location.span.length == 6


def test_lex_unclosed_multiline_string_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string('"hello\nworld')
    err = exc_info.value.errors[0]
    assert err.kind == ErrorKind.SYNTAX
    assert err.location.span.offset == 0
    assert err.location.span.length == 12


def test_lex_missing_method_name_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string("Foo::")
    assert len(exc_info.value.errors) == 1
    assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX
    assert "method name" in exc_info.value.errors[0].message


# ---------------------------------------------------------------------------
# Escape sequences
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "source,expected_char",
    [
        (r'"hello\nworld"', '"hello\nworld"'),
        (r'"hello\tworld"', '"hello\tworld"'),
        (r'"hello\\world"', '"hello\\world"'),
        (r'"hello\"world"', '"hello"world"'),
        (r'"hello\0world"', '"hello\0world"'),
        (r'"hello\rworld"', '"hello\rworld"'),
    ],
)
def test_lex_escape_sequences(source, expected_char):
    tokens = lex_string(source)
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == expected_char


def test_lex_multiple_escapes_in_one_string():
    tokens = lex_string(r'"a\nb\tc"')
    assert tokens[0].value == '"a\nb\tc"'


def test_lex_escaped_quote_does_not_terminate():
    tokens = lex_string(r'"say \"hi\""')
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == '"say "hi""'


def test_lex_invalid_escape_raises():
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string(r'"bad\q"')
    err = exc_info.value.errors[0]
    assert err.kind == ErrorKind.SYNTAX
    assert "Invalid escape sequence" in err.message
    assert err.location.span.offset == 4
    assert err.location.span.length == 2


def test_lex_backslash_at_end_unclosed():
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string('"hello\\')
    err = exc_info.value.errors[0]
    assert err.kind == ErrorKind.SYNTAX
    assert "Unclosed string" in err.message


def test_lex_escape_span_reflects_source_length():
    """Token span length should reflect source length, not processed length."""
    tokens = lex_string(r'"a\nb"')
    # Source is 6 chars: " a \ n b "
    assert tokens[0].location.span.offset == 0
    assert tokens[0].location.span.length == 6


def test_lex_string_only_escape():
    tokens = lex_string(r'"\n"')
    assert tokens[0].value == '"\n"'


def test_lex_escape_at_start():
    tokens = lex_string(r'"\nhello"')
    assert tokens[0].value == '"\nhello"'


def test_lex_escape_at_end():
    tokens = lex_string(r'"hello\n"')
    assert tokens[0].value == '"hello\n"'


def test_lex_adjacent_backslash_then_n():
    """\\\\n in source becomes backslash followed by literal n."""
    tokens = lex_string(r'"a\\nb"')
    assert tokens[0].value == '"a\\nb"'


def test_lex_multiple_invalid_escapes():
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string(r'"bad\q\x"')
    errors = exc_info.value.errors
    assert len(errors) == 2
    assert all(e.kind == ErrorKind.SYNTAX for e in errors)
    assert "\\q" in errors[0].message
    assert "\\x" in errors[1].message


def test_lex_escape_span_multiple_escapes():
    tokens = lex_string(r'"a\nb\tc"')
    # Source: " a \ n b \ t c " = 9 chars
    assert tokens[0].location.span.length == 9


# ---------------------------------------------------------------------------
# F-strings
# ---------------------------------------------------------------------------
def test_lex_fstring_plain_text():
    """f"hello" produces FSTRING_START, FSTRING_TEXT, FSTRING_END."""
    tokens = lex_string('f"hello"')
    kinds = [t.kind for t in tokens if t.kind != TokenKind.EOF]
    assert kinds == [
        TokenKind.FSTRING_START,
        TokenKind.FSTRING_TEXT,
        TokenKind.FSTRING_END,
    ]
    assert tokens[1].value == "hello"


def test_lex_fstring_empty():
    """f"" produces FSTRING_START, FSTRING_END (no text part)."""
    tokens = lex_string('f""')
    kinds = [t.kind for t in tokens if t.kind != TokenKind.EOF]
    assert kinds == [TokenKind.FSTRING_START, TokenKind.FSTRING_END]


def test_lex_fstring_expression_only():
    """f"{x}" produces FSTRING_START, EXPR_START, IDENTIFIER, EXPR_END, FSTRING_END."""
    tokens = lex_string('f"{x}"')
    kinds = [t.kind for t in tokens if t.kind != TokenKind.EOF]
    assert kinds == [
        TokenKind.FSTRING_START,
        TokenKind.FSTRING_EXPR_START,
        TokenKind.IDENTIFIER,
        TokenKind.FSTRING_EXPR_END,
        TokenKind.FSTRING_END,
    ]
    id_token = [t for t in tokens if t.kind == TokenKind.IDENTIFIER][0]
    assert id_token.value == "x"


def test_lex_fstring_text_and_expression():
    """f"hello {x} world" produces correct token sequence."""
    tokens = lex_string('f"hello {x} world"')
    kinds = [t.kind for t in tokens if t.kind != TokenKind.EOF]
    assert kinds == [
        TokenKind.FSTRING_START,
        TokenKind.FSTRING_TEXT,
        TokenKind.FSTRING_EXPR_START,
        TokenKind.IDENTIFIER,
        TokenKind.FSTRING_EXPR_END,
        TokenKind.FSTRING_TEXT,
        TokenKind.FSTRING_END,
    ]
    text_tokens = [t for t in tokens if t.kind == TokenKind.FSTRING_TEXT]
    assert text_tokens[0].value == "hello "
    assert text_tokens[1].value == " world"


def test_lex_fstring_escaped_braces():
    r"""f"\{braces\}" produces FSTRING_TEXT with literal {braces}."""
    tokens = lex_string(r'f"\{braces\}"')
    kinds = [t.kind for t in tokens if t.kind != TokenKind.EOF]
    assert kinds == [
        TokenKind.FSTRING_START,
        TokenKind.FSTRING_TEXT,
        TokenKind.FSTRING_END,
    ]
    assert tokens[1].value == "{braces}"


def test_lex_fstring_escape_sequences():
    r"""f"hello\nworld" processes escape sequences inside f-strings."""
    tokens = lex_string(r'f"hello\nworld"')
    text_tokens = [t for t in tokens if t.kind == TokenKind.FSTRING_TEXT]
    assert len(text_tokens) == 1
    assert text_tokens[0].value == "hello\nworld"


def test_lex_fstring_multiple_expressions():
    """f"{a} and {b}" produces correct tokens for multiple expressions."""
    tokens = lex_string('f"{a} and {b}"')
    kinds = [t.kind for t in tokens if t.kind != TokenKind.EOF]
    assert kinds == [
        TokenKind.FSTRING_START,
        TokenKind.FSTRING_EXPR_START,
        TokenKind.IDENTIFIER,
        TokenKind.FSTRING_EXPR_END,
        TokenKind.FSTRING_TEXT,
        TokenKind.FSTRING_EXPR_START,
        TokenKind.IDENTIFIER,
        TokenKind.FSTRING_EXPR_END,
        TokenKind.FSTRING_END,
    ]
    text_tokens = [t for t in tokens if t.kind == TokenKind.FSTRING_TEXT]
    assert text_tokens[0].value == " and "


def test_lex_fstring_complex_expression():
    """f"{1 2 +}" produces tokens for a multi-token expression."""
    tokens = lex_string('f"{1 2 +}"')
    kinds = [t.kind for t in tokens if t.kind != TokenKind.EOF]
    assert kinds == [
        TokenKind.FSTRING_START,
        TokenKind.FSTRING_EXPR_START,
        TokenKind.LITERAL,
        TokenKind.LITERAL,
        TokenKind.OPERATOR,
        TokenKind.FSTRING_EXPR_END,
        TokenKind.FSTRING_END,
    ]


def test_lex_fstring_unclosed_raises():
    """Unclosed f-string raises a syntax error."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string('f"hello')
    err = exc_info.value.errors[0]
    assert err.kind == ErrorKind.SYNTAX


def test_lex_fstring_unclosed_expression_raises():
    """Unclosed expression in f-string raises a syntax error."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string('f"hello {x"')
    err = exc_info.value.errors[0]
    assert err.kind == ErrorKind.SYNTAX
