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
    "syscall0",
    "syscall1",
    "syscall2",
    "syscall3",
    "syscall4",
    "syscall5",
    "syscall6",
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


# ---------------------------------------------------------------------------
# Arrow (`->`) handling in peek_word
# ---------------------------------------------------------------------------
def test_lex_arrow_standalone():
    """Standalone -> is a delimiter."""
    tokens = lex_string("->")
    assert tokens[0].kind == TokenKind.DELIMITER
    assert tokens[0].value == "->"


def test_lex_arrow_with_spaces():
    """-> surrounded by spaces produces three tokens."""
    tokens = lex_string("a -> b")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 3
    assert non_eof[0].value == "a"
    assert non_eof[1].kind == TokenKind.DELIMITER
    assert non_eof[1].value == "->"
    assert non_eof[2].value == "b"


def test_lex_arrow_after_identifier():
    """a-> should split into identifier 'a' and delimiter '->'."""
    tokens = lex_string("a->b")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert non_eof[0].value == "a"
    assert non_eof[0].kind == TokenKind.IDENTIFIER
    assert non_eof[1].kind == TokenKind.DELIMITER
    assert non_eof[1].value == "->"
    assert non_eof[2].value == "b"
    assert non_eof[2].kind == TokenKind.IDENTIFIER


def test_lex_arrow_in_function_signature():
    """Arrow in fn signature: fn foo x:int -> int."""
    tokens = lex_string("fn foo x:int -> int")
    values = [t.value for t in tokens if t.kind != TokenKind.EOF]
    assert "->" in values
    arrow_idx = values.index("->")
    assert tokens[arrow_idx].kind == TokenKind.DELIMITER


def test_lex_arrow_setter_syntax():
    """Arrow setter: value person->name splits correctly."""
    tokens = lex_string('"Jane" person->name')
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert non_eof[0].kind == TokenKind.LITERAL
    assert non_eof[0].value == '"Jane"'
    assert non_eof[1].value == "person"
    assert non_eof[2].kind == TokenKind.DELIMITER
    assert non_eof[2].value == "->"
    assert non_eof[3].value == "name"


# ---------------------------------------------------------------------------
# Namespace (::) edge cases
# ---------------------------------------------------------------------------
def test_lex_namespace_basic():
    """Type::method produces a single IDENTIFIER token."""
    tokens = lex_string("Foo::bar")
    assert tokens[0].kind == TokenKind.IDENTIFIER
    assert tokens[0].value == "Foo::bar"


def test_lex_namespace_chained():
    """Nested namespace A::B::c produces a single token."""
    tokens = lex_string("A::B::c")
    assert tokens[0].kind == TokenKind.IDENTIFIER
    assert tokens[0].value == "A::B::c"


def test_lex_namespace_in_expression():
    """Namespace call in an expression context."""
    tokens = lex_string("42 Foo::bar")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 2
    assert non_eof[0].kind == TokenKind.LITERAL
    assert non_eof[1].kind == TokenKind.IDENTIFIER
    assert non_eof[1].value == "Foo::bar"


def test_lex_namespace_missing_method_eof():
    """Trailing :: at EOF raises syntax error."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string("Foo::")
    assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX


def test_lex_namespace_missing_method_space():
    """:: followed by space and then identifier raises syntax error."""
    with pytest.raises(CasaErrorCollection) as exc_info:
        lex_string("Foo:: bar")
    assert exc_info.value.errors[0].kind == ErrorKind.SYNTAX


# ---------------------------------------------------------------------------
# Whitespace handling
# ---------------------------------------------------------------------------
def test_lex_whitespace_only():
    """Input of only whitespace produces just EOF."""
    tokens = lex_string("   \t\n  ")
    assert len(tokens) == 1
    assert tokens[0].kind == TokenKind.EOF


def test_lex_multiple_whitespace_between_tokens():
    """Multiple spaces/tabs between tokens are skipped correctly."""
    tokens = lex_string("42   \t   hello")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 2
    assert non_eof[0].value == "42"
    assert non_eof[1].value == "hello"


def test_lex_leading_trailing_whitespace():
    """Leading and trailing whitespace is ignored."""
    tokens = lex_string("   42   ")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 1
    assert non_eof[0].value == "42"


def test_lex_newlines_between_tokens():
    """Newlines separate tokens properly."""
    tokens = lex_string("42\n43\n44")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 3
    assert [t.value for t in non_eof] == ["42", "43", "44"]


# ---------------------------------------------------------------------------
# Long inputs (verify no O(n^2) regression)
# ---------------------------------------------------------------------------
def test_lex_long_string():
    """Long string literal lexes without error."""
    content = "a" * 5000
    tokens = lex_string(f'"{content}"')
    assert tokens[0].kind == TokenKind.LITERAL
    assert tokens[0].value == f'"{content}"'


def test_lex_long_identifier():
    """Long identifier lexes without error."""
    name = "x" * 5000
    tokens = lex_string(name)
    assert tokens[0].kind == TokenKind.IDENTIFIER
    assert tokens[0].value == name


def test_lex_many_tokens():
    """Many tokens in sequence lex correctly."""
    source = " ".join(str(i) for i in range(1000))
    tokens = lex_string(source)
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 1000
    assert all(t.kind == TokenKind.LITERAL for t in non_eof)


# ---------------------------------------------------------------------------
# Delimiter.from_str and Operator.from_str correctness
# ---------------------------------------------------------------------------
def test_delimiter_from_str_all_values():
    """Delimiter.from_str returns correct enum for every delimiter string."""
    expected = {
        "->": Delimiter.ARROW,
        ",": Delimiter.COMMA,
        ":": Delimiter.COLON,
        ".": Delimiter.DOT,
        "#": Delimiter.HASHTAG,
        "{": Delimiter.OPEN_BRACE,
        "}": Delimiter.CLOSE_BRACE,
        "[": Delimiter.OPEN_BRACKET,
        "]": Delimiter.CLOSE_BRACKET,
        "(": Delimiter.OPEN_PAREN,
        ")": Delimiter.CLOSE_PAREN,
    }
    for string, delim in expected.items():
        assert Delimiter.from_str(string) == delim


def test_delimiter_from_str_returns_none_for_unknown():
    """Delimiter.from_str returns None for non-delimiter strings."""
    assert Delimiter.from_str("x") is None
    assert Delimiter.from_str("") is None
    assert Delimiter.from_str("abc") is None
    assert Delimiter.from_str("+") is None


def test_operator_from_str_all_values():
    """Operator.from_str returns correct enum for every operator string."""
    expected = {
        "+": Operator.PLUS,
        "-": Operator.MINUS,
        "*": Operator.MULTIPLICATION,
        "/": Operator.DIVISION,
        "%": Operator.MODULO,
        "<<": Operator.SHL,
        ">>": Operator.SHR,
        "&&": Operator.AND,
        "||": Operator.OR,
        "!": Operator.NOT,
        "==": Operator.EQ,
        ">=": Operator.GE,
        ">": Operator.GT,
        "<=": Operator.LE,
        "<": Operator.LT,
        "!=": Operator.NE,
        "=": Operator.ASSIGN,
        "-=": Operator.ASSIGN_DECREMENT,
        "+=": Operator.ASSIGN_INCREMENT,
    }
    for string, op in expected.items():
        assert Operator.from_str(string) == op


def test_operator_from_str_returns_none_for_unknown():
    """Operator.from_str returns None for non-operator strings."""
    assert Operator.from_str("x") is None
    assert Operator.from_str("") is None
    assert Operator.from_str("**") is None
    assert Operator.from_str("=>") is None


# ---------------------------------------------------------------------------
# Delimiter.from_str and Operator.from_str consistency
# ---------------------------------------------------------------------------
def test_delimiter_from_str_consistent_with_enum():
    """Every Delimiter enum member has a from_str mapping."""
    all_strings = ["->", ",", ":", ".", "#", "{", "}", "[", "]", "(", ")"]
    assert len(all_strings) == len(Delimiter)
    for s in all_strings:
        assert Delimiter.from_str(s) is not None


def test_operator_from_str_consistent_with_enum():
    """Every Operator enum member has a from_str mapping."""
    all_strings = [
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
    assert len(all_strings) == len(Operator)
    for s in all_strings:
        assert Operator.from_str(s) is not None


# ---------------------------------------------------------------------------
# Function reference (&)
# ---------------------------------------------------------------------------
def test_lex_function_reference():
    """&name produces an IDENTIFIER token."""
    tokens = lex_string("&foo")
    assert tokens[0].kind == TokenKind.IDENTIFIER
    assert tokens[0].value == "&foo"


# ---------------------------------------------------------------------------
# Mixed token sequences
# ---------------------------------------------------------------------------
def test_lex_struct_instantiation():
    """Struct instantiation: fields then struct name."""
    tokens = lex_string('18 "John" Person')
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 3
    assert non_eof[0].kind == TokenKind.LITERAL
    assert non_eof[0].value == "18"
    assert non_eof[1].kind == TokenKind.LITERAL
    assert non_eof[1].value == '"John"'
    assert non_eof[2].kind == TokenKind.IDENTIFIER
    assert non_eof[2].value == "Person"


def test_lex_dot_accessor():
    """Dot in accessor context: person.name."""
    tokens = lex_string("person.name")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert non_eof[0].value == "person"
    assert non_eof[1].kind == TokenKind.DELIMITER
    assert non_eof[1].value == "."
    assert non_eof[2].value == "name"


def test_lex_type_cast():
    """Type cast (TypeName) produces delimiter and identifier tokens."""
    tokens = lex_string("(SomeType)")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert non_eof[0].kind == TokenKind.DELIMITER
    assert non_eof[0].value == "("
    assert non_eof[1].kind == TokenKind.IDENTIFIER
    assert non_eof[1].value == "SomeType"
    assert non_eof[2].kind == TokenKind.DELIMITER
    assert non_eof[2].value == ")"


def test_lex_array_literal():
    """Array literal [1, 2, 3] produces correct tokens."""
    tokens = lex_string("[1, 2, 3]")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    values = [t.value for t in non_eof]
    assert values == ["[", "1", ",", "2", ",", "3", "]"]


def test_lex_lambda_block():
    """Lambda { body } produces delimiter and body tokens."""
    tokens = lex_string("{ 2 * }")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert non_eof[0].kind == TokenKind.DELIMITER
    assert non_eof[0].value == "{"
    assert non_eof[-1].kind == TokenKind.DELIMITER
    assert non_eof[-1].value == "}"


# ---------------------------------------------------------------------------
# Comment edge cases
# ---------------------------------------------------------------------------
def test_lex_comment_at_end_of_file():
    """Comment at EOF with no trailing newline."""
    tokens = lex_string("42 # trailing comment")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 1
    assert non_eof[0].value == "42"


def test_lex_empty_comment():
    """Just a # with nothing after it."""
    tokens = lex_string("#")
    assert len(tokens) == 1
    assert tokens[0].kind == TokenKind.EOF


def test_lex_multiple_comment_lines():
    """Multiple lines with comments interspersed."""
    tokens = lex_string("1\n# comment\n2\n# another\n3")
    non_eof = [t for t in tokens if t.kind != TokenKind.EOF]
    assert len(non_eof) == 3
    assert [t.value for t in non_eof] == ["1", "2", "3"]
