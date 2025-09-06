import itertools
from dataclasses import dataclass
from pathlib import Path

from .common import Intrinsic, Location, Span, Token, TokenKind


@dataclass
class Lexer:
    cursor: int
    code: str
    file: Path

    def lex(self) -> list[Token]:
        tokens = []
        while token := self.parse_token():
            tokens.append(token)
        assert self.is_finished(), "Lexer did not finish parsing"

        tokens.append(Token("", TokenKind.EOF, self.get_location(0)))
        return tokens

    def rest(self) -> str:
        return self.code[self.cursor :]

    def is_finished(self) -> bool:
        return self.cursor >= len(self.code)

    def is_whitespace(self) -> bool:
        if char := self.peek_char():
            return char.isspace()
        return False

    def skip_whitespace(self):
        rest = self.rest()
        self.cursor += len(rest) - len(rest.lstrip())

    def peek_char(self) -> str | None:
        try:
            return self.code[self.cursor]
        except IndexError:
            return None

    def peek_word(self) -> str | None:
        if self.is_finished() or self.is_whitespace():
            return None
        return self.rest().split(maxsplit=1)[0]

    def parse_token(self) -> Token | None:
        assert len(TokenKind) == 4, "Exhaustive handling for `TokenKind`"

        self.skip_whitespace()
        match c := self.peek_char():
            case None:
                return None
            case "+":
                return self.lex_token(c, TokenKind.OPERATOR)
            case c if c.isdigit():
                return self.lex_integer_literal()
            case _:
                return self.lex_multichar_token()

    def get_location(self, span_length: int) -> Location:
        return Location(self.file, Span(self.cursor, span_length))

    def lex_token(self, char: str, token_kind: TokenKind) -> Token:
        token = Token(char, token_kind, self.get_location(1))
        self.cursor += 1
        return token

    def lex_multichar_token(self) -> Token | None:
        value = self.peek_word()
        if not value:
            return None

        value_len = len(value)
        location = self.get_location(value_len)
        self.cursor += value_len

        if Intrinsic.from_lowercase(value):
            return Token(value, TokenKind.INTRINSIC, location)
        raise NotImplementedError(value)

    def lex_integer_literal(self) -> Token:
        digits = "".join(itertools.takewhile(str.isdigit, self.rest()))
        assert digits, "Could not parse integer literal"

        digit_count = len(digits)
        location = Location(self.file, Span(self.cursor, digit_count))
        self.cursor += digit_count
        return Token(digits, TokenKind.LITERAL, location)


def lex_file(file: Path) -> list[Token]:
    with open(file, "r") as code_file:
        code = code_file.read()
    lexer = Lexer(code=code, cursor=0, file=file)
    return lexer.lex()
