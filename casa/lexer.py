from dataclasses import dataclass, field
from pathlib import Path

from casa.common import (
    Cursor,
    Delimiter,
    Intrinsic,
    Keyword,
    Location,
    Operator,
    Span,
    Token,
    TokenKind,
)
from casa.error import SOURCE_CACHE, CasaError, CasaErrorCollection, ErrorKind

ESCAPE_SEQUENCES = {
    "n": "\n",
    "t": "\t",
    "\\": "\\",
    '"': '"',
    "0": "\0",
    "r": "\r",
    "{": "{",
    "}": "}",
    "'": "'",
}


@dataclass(slots=True)
class Lexer:
    cursor: Cursor[str]
    file: Path
    errors: list[CasaError] = field(default_factory=list)
    _source: str = field(init=False)

    def __post_init__(self):
        assert isinstance(self.cursor.sequence, str)
        self._source = self.cursor.sequence

    def lex(self) -> list[Token]:
        tokens: list[Token] = []
        while not self.cursor.is_finished():
            self.skip_whitespace()
            if self.cursor.is_finished():
                break
            if self.startswith('f"'):
                tokens.extend(self.parse_fstring())
                continue
            if token := self.parse_token():
                tokens.append(token)

        tokens.append(Token("", TokenKind.EOF, self.current_location(0)))

        if self.errors:
            raise CasaErrorCollection(self.errors)

        return tokens

    def current_location(self, span_length: int) -> Location:
        return Location(self.file, Span(self.cursor.position, span_length))

    def peek_word(self) -> str | None:
        if self.cursor.is_finished():
            return None

        seq = self._source
        word = ""
        pos = self.cursor.position
        while pos < len(seq):
            char = seq[pos]
            if Delimiter.from_str(char):
                break
            if char.isspace():
                break
            if word == "->":
                break
            if word.endswith("->"):
                word = word[:-2]
                break
            word += char
            pos += 1

        return word or None

    def startswith(self, prefix: str) -> bool:
        return self._source.startswith(prefix, self.cursor.position)

    def skip_whitespace(self):
        seq = self._source
        pos = self.cursor.position
        while pos < len(seq) and seq[pos].isspace():
            pos += 1
        self.cursor.position = pos

    def skip_line(self):
        idx = self._source.find("\n", self.cursor.position)
        if idx != -1:
            self.cursor.position = idx + 1
        else:
            self.cursor.position = len(self._source)

    def lex_integer_literal(self) -> Token:
        seq = self._source
        start = self.cursor.position
        pos = start
        while pos < len(seq) and seq[pos].isdigit():
            pos += 1
        assert pos > start, "Could not parse integer literal"

        digit_count = pos - start
        location = Location(self.file, Span(start, digit_count))
        self.cursor.position = pos
        return Token(seq[start:pos], TokenKind.LITERAL, location)

    def parse_fstring(self) -> list[Token]:
        start = self.cursor.position
        start_loc = self.current_location(2)
        self.cursor.position += 2  # skip f"

        tokens: list[Token] = [Token("", TokenKind.FSTRING_START, start_loc)]
        text = ""
        text_start = self.cursor.position

        def flush_text():
            nonlocal text
            if not text:
                return
            loc = Location(
                self.file, Span(text_start, self.cursor.position - text_start)
            )
            tokens.append(Token(text, TokenKind.FSTRING_TEXT, loc))
            text = ""

        while not self.cursor.is_finished():
            char = self.cursor.peek()
            assert char is not None

            if char == "\\":
                self.cursor.position += 1
                escaped = self.parse_escape_sequence()
                if escaped is None:
                    break
                text += escaped
                continue

            if char == "{":
                flush_text()
                expr_start = self.cursor.position
                expr_loc = Location(self.file, Span(expr_start, 1))
                tokens.append(Token("{", TokenKind.FSTRING_EXPR_START, expr_loc))
                self.cursor.position += 1
                tokens.extend(self._lex_fstring_expr())
                text_start = self.cursor.position
                continue

            if char == '"':
                flush_text()
                self.cursor.position += 1
                end_loc = Location(self.file, Span(self.cursor.position - 1, 1))
                tokens.append(Token("", TokenKind.FSTRING_END, end_loc))
                return tokens

            text += char
            self.cursor.position += 1

        span_length = self.cursor.position - start
        self.errors.append(
            CasaError(
                ErrorKind.SYNTAX,
                "Unclosed f-string",
                Location(self.file, Span(start, span_length)),
            )
        )
        return tokens

    def _lex_fstring_expr(self) -> list[Token]:
        tokens: list[Token] = []
        depth = 0

        while not self.cursor.is_finished():
            self.skip_whitespace()
            if self.cursor.is_finished():
                break

            char = self.cursor.peek()

            if self.startswith('f"'):
                tokens.extend(self.parse_fstring())
                continue

            if char == "}" and depth == 0:
                end_loc = self.current_location(1)
                tokens.append(Token("}", TokenKind.FSTRING_EXPR_END, end_loc))
                self.cursor.position += 1
                return tokens

            if char == "{":
                depth += 1
            elif char == "}":
                depth -= 1

            token = self.parse_token()
            if token:
                tokens.append(token)

        self.errors.append(
            CasaError(
                ErrorKind.SYNTAX,
                "Unclosed f-string expression",
                self.current_location(0),
            )
        )
        return tokens

    def parse_token(self) -> Token | None:
        match c := self.cursor.peek():
            case None:
                return None
            case "#":
                self.skip_line()
                return None
            case "'":
                return self.parse_char_literal()
            case c if Delimiter.from_str(c):
                return self.lex_token(c, TokenKind.DELIMITER)
            case c if c.isdigit():
                return self.lex_integer_literal()
            case _:
                return self.lex_multichar_token()

    def lex_token(self, char: str, token_kind: TokenKind) -> Token:
        token = Token(char, token_kind, self.current_location(1))
        self.cursor.position += 1
        return token

    def lex_multichar_token(self) -> Token | None:
        original_position = self.cursor.position
        if self.startswith('"'):
            string_literal = self.parse_string_literal()
            span_length = self.cursor.position - original_position
            location = Location(self.file, Span(original_position, span_length))
            return Token(string_literal, TokenKind.LITERAL, location)

        value = self.peek_word()
        if not value:
            return None

        value_len = len(value)
        location = self.current_location(value_len)
        self.cursor.position += value_len

        # Negative integer literal is not preceded by digit
        is_previous_digit = (
            original_position > 0 and self._source[original_position - 1].isdigit()
        )
        if is_negative_integer_literal(value) and not is_previous_digit:
            return Token(value, TokenKind.LITERAL, location)
        if value in ("true", "false"):
            return Token(value, TokenKind.LITERAL, location)
        if value in ("none", "some"):
            return Token(value, TokenKind.LITERAL, location)
        if Delimiter.from_str(value):
            return Token(value, TokenKind.DELIMITER, location)
        if Intrinsic.from_lowercase(value):
            return Token(value, TokenKind.INTRINSIC, location)
        if Keyword.from_lowercase(value):
            return Token(value, TokenKind.KEYWORD, location)
        if Operator.from_str(value):
            return Token(value, TokenKind.OPERATOR, location)
        if self.startswith("::"):
            self.cursor.position += 2
            method = self.lex_multichar_token()
            if not method:
                self.errors.append(
                    CasaError(
                        ErrorKind.SYNTAX,
                        "Expected method name after `::`",
                        location,
                    )
                )
                return Token(value, TokenKind.IDENTIFIER, location)
            return Token(f"{value}::{method.value}", TokenKind.IDENTIFIER, location)

        return Token(value, TokenKind.IDENTIFIER, location)

    def parse_escape_sequence(self) -> str | None:
        next_char = self.cursor.pop()
        if next_char is None:
            return None
        if next_char in ESCAPE_SEQUENCES:
            return ESCAPE_SEQUENCES[next_char]
        esc_offset = self.cursor.position - 2
        self.errors.append(
            CasaError(
                ErrorKind.SYNTAX,
                f"Invalid escape sequence `\\{next_char}`",
                Location(self.file, Span(esc_offset, 2)),
            )
        )
        return ""

    def parse_string_literal(self) -> str:
        start = self.cursor.position
        assert self.cursor.pop() == '"', 'String literal starts with `"`'

        string_literal = '"'
        closed = False
        while char := self.cursor.pop():
            if char == "\\":
                escaped = self.parse_escape_sequence()
                if escaped is None:
                    break
                string_literal += escaped
                continue
            if char == '"':
                string_literal += char
                closed = True
                break
            string_literal += char

        if not closed:
            span_length = self.cursor.position - start
            self.errors.append(
                CasaError(
                    ErrorKind.SYNTAX,
                    "Unclosed string literal",
                    Location(self.file, Span(start, span_length)),
                )
            )

        return string_literal

    def parse_char_literal(self) -> Token:
        start = self.cursor.position
        assert self.cursor.pop() == "'", "Char literal starts with `'`"

        char_value: str | None = None
        next_char = self.cursor.pop()
        if next_char is None:
            span_length = self.cursor.position - start
            self.errors.append(
                CasaError(
                    ErrorKind.SYNTAX,
                    "Unclosed char literal",
                    Location(self.file, Span(start, span_length)),
                )
            )
            return Token(
                "'\0'", TokenKind.LITERAL, Location(self.file, Span(start, span_length))
            )

        if next_char == "\\":
            escaped = self.parse_escape_sequence()
            if escaped is None:
                span_length = self.cursor.position - start
                self.errors.append(
                    CasaError(
                        ErrorKind.SYNTAX,
                        "Unclosed char literal",
                        Location(self.file, Span(start, span_length)),
                    )
                )
                return Token(
                    "'\0'",
                    TokenKind.LITERAL,
                    Location(self.file, Span(start, span_length)),
                )
            char_value = escaped
        elif next_char == "'":
            span_length = self.cursor.position - start
            self.errors.append(
                CasaError(
                    ErrorKind.SYNTAX,
                    "Empty char literal",
                    Location(self.file, Span(start, span_length)),
                )
            )
            return Token(
                "'\0'", TokenKind.LITERAL, Location(self.file, Span(start, span_length))
            )
        else:
            char_value = next_char

        closing = self.cursor.pop()
        if closing != "'":
            span_length = self.cursor.position - start
            self.errors.append(
                CasaError(
                    ErrorKind.SYNTAX,
                    "Unclosed char literal",
                    Location(self.file, Span(start, span_length)),
                )
            )

        span_length = self.cursor.position - start
        location = Location(self.file, Span(start, span_length))
        return Token(f"'{char_value}'", TokenKind.LITERAL, location)


def is_negative_integer_literal(value: str) -> bool:
    return len(value) > 1 and value[0] == "-" and value[1:].isdigit()


def lex_file(file: Path) -> list[Token]:
    with open(file, "r", encoding="utf-8") as code_file:
        code = code_file.read()
    SOURCE_CACHE[file] = code
    lexer = Lexer(file=file, cursor=Cursor(sequence=code))
    return lexer.lex()
