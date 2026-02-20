import itertools
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


@dataclass(slots=True)
class Lexer:
    cursor: Cursor[str]
    file: Path
    errors: list[CasaError] = field(default_factory=list)

    def lex(self) -> list[Token]:
        tokens: list[Token] = []
        while not self.cursor.is_finished():
            if token := self.parse_token():
                tokens.append(token)

        tokens.append(Token("", TokenKind.EOF, self.current_location(0)))

        if self.errors:
            raise CasaErrorCollection(self.errors)

        return tokens

    def rest(self) -> str:
        return self.cursor.sequence[self.cursor.position :]  # type: ignore

    def current_location(self, span_length: int) -> Location:
        return Location(self.file, Span(self.cursor.position, span_length))

    def expect_char(self, char: str) -> bool:
        return char == self.cursor.pop()

    def expect_startswith(self, prefix: str) -> bool:
        if self.startswith(prefix):
            self.cursor.position += len(prefix)
            return True
        return False

    def peek_word(self) -> str | None:
        if self.cursor.is_finished():
            return None

        word = ""
        for char in self.rest():
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

        return word or None

    def startswith(self, prefix: str) -> bool:
        return self.rest().startswith(prefix)

    def skip_whitespace(self):
        rest = self.rest()
        self.cursor.position += len(rest) - len(rest.lstrip())

    def skip_line(self):
        s = self.rest()
        newline_index = s.find("\n")
        if newline_index != -1:
            self.cursor.position += newline_index + 1
        else:
            self.cursor.position += len(s)

    def is_whitespace(self) -> bool:
        if char := self.cursor.peek():
            return char.isspace()
        return False

    def lex_integer_literal(self) -> Token:
        digits = "".join(itertools.takewhile(str.isdigit, self.rest()))
        assert digits, "Could not parse integer literal"

        digit_count = len(digits)
        location = Location(self.file, Span(self.cursor.position, digit_count))
        self.cursor.position += digit_count
        return Token(digits, TokenKind.LITERAL, location)

    def parse_token(self) -> Token | None:
        self.skip_whitespace()
        match c := self.cursor.peek():
            case None:
                return None
            case "#":
                self.skip_line()
                return None
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
            loc = Location(self.file, Span(original_position, len(string_literal)))
            return Token(string_literal, TokenKind.LITERAL, loc)

        value = self.peek_word()
        if not value:
            return None

        value_len = len(value)
        location = self.current_location(value_len)
        self.cursor.position += value_len

        # Negative integer literal is not preceded by digit
        is_previous_digit = (
            original_position > 0
            and self.cursor.sequence[original_position - 1].isdigit()
        )
        if is_negative_integer_literal(value) and not is_previous_digit:
            return Token(value, TokenKind.LITERAL, location)
        if value in ("true", "false"):
            return Token(value, TokenKind.LITERAL, location)
        if Delimiter.from_str(value):
            return Token(value, TokenKind.DELIMITER, location)
        if Intrinsic.from_lowercase(value):
            return Token(value, TokenKind.INTRINSIC, location)
        if Keyword.from_lowercase(value):
            return Token(value, TokenKind.KEYWORD, location)
        if Operator.from_str(value):
            return Token(value, TokenKind.OPERATOR, location)
        if self.expect_startswith("::"):
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

    def parse_string_literal(self) -> str:
        start = self.cursor.position
        assert self.expect_char('"'), 'String literal starts with `"`'

        string_literal = '"'
        while char := self.cursor.pop():
            string_literal += char
            if char == '"':
                break
        else:
            span_length = self.cursor.position - start
            self.errors.append(
                CasaError(
                    ErrorKind.SYNTAX,
                    "Unclosed string literal",
                    Location(self.file, Span(start, span_length)),
                )
            )
            return string_literal

        return string_literal


def is_negative_integer_literal(value: str):
    return len(value) > 1 and value[0] == "-" and value[1:].isdigit()


def lex_file(file: Path) -> list[Token]:
    with open(file, "r") as code_file:
        code = code_file.read()
    SOURCE_CACHE[file] = code
    lexer = Lexer(file=file, cursor=Cursor(sequence=code))
    return lexer.lex()
