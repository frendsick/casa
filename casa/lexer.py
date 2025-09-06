import itertools
from dataclasses import dataclass
from pathlib import Path

from casa.common import Cursor, Intrinsic, Location, Span, Token, TokenKind


@dataclass(slots=True)
class Lexer:
    cursor: Cursor[str]
    file: Path

    def lex(self) -> list[Token]:
        tokens: list[Token] = []
        while (tok := self.parse_token()) is not None:
            tokens.append(tok)

        tokens.append(Token("", TokenKind.EOF, self.current_location(0)))
        return tokens

    def rest(self) -> str:
        return self.cursor.sequence[self.cursor.position :]  # type: ignore

    def current_location(self, span_length: int) -> Location:
        return Location(self.file, Span(self.cursor.position, span_length))

    def peek_word(self) -> str | None:
        if self.cursor.is_finished():
            return None
        return self.rest().split(maxsplit=1)[0]

    def skip_whitespace(self):
        rest = self.rest()
        self.cursor.position += len(rest) - len(rest.lstrip())

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
        assert len(TokenKind) == 4, "Exhaustive handling for `TokenKind`"

        self.skip_whitespace()
        match c := self.cursor.peek():
            case None:
                return None
            case "+":
                return self.lex_token(c, TokenKind.OPERATOR)
            case c if c.isdigit():
                return self.lex_integer_literal()
            case _:
                return self.lex_multichar_token()

    def lex_token(self, char: str, token_kind: TokenKind) -> Token:
        token = Token(char, token_kind, self.current_location(1))
        self.cursor.position += 1
        return token

    def lex_multichar_token(self) -> Token | None:
        value = self.peek_word()
        if not value:
            return None

        value_len = len(value)
        location = self.current_location(value_len)
        self.cursor.position += value_len

        if Intrinsic.from_lowercase(value):
            return Token(value, TokenKind.INTRINSIC, location)
        raise NotImplementedError(value)


def lex_file(file: Path) -> list[Token]:
    with open(file, "r") as code_file:
        code = code_file.read()
    lexer = Lexer(file=file, cursor=Cursor(sequence=code))
    return lexer.lex()
