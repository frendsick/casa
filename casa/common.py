from dataclasses import dataclass
from enum import Enum, auto
from pathlib import Path
from typing import Self


class TokenKind(Enum):
    # DELIMITER = auto()
    EOF = auto()
    # IDENTIFIER = auto()
    INTRINSIC = auto()
    # KEYWORD = auto()
    LITERAL = auto()
    OPERATOR = auto()


class Intrinsic(Enum):
    PRINT = auto()

    @classmethod
    def from_lowercase(cls, value: str) -> Self | None:
        if not value.islower():
            return None
        return cls.__members__.get(value.upper())


@dataclass
class Span:
    offset: int
    length: int


@dataclass
class Location:
    file: Path
    span: Span


@dataclass
class Token:
    value: str
    kind: TokenKind
    location: Location
