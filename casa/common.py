from dataclasses import dataclass
from enum import Enum, auto
from pathlib import Path
from typing import Any, Self, assert_never


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


class Operator(Enum):
    PLUS = auto()


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


class OpKind(Enum):
    ADD = auto()
    PUSH_INT = auto()
    PRINT = auto()


@dataclass
class Op:
    value: Any
    kind: OpKind
    location: Location

    def __post_init__(self):
        assert len(OpKind) == 3, "Exhaustive handling for `OpKind`"
        match self.kind:
            # Requires `int`
            case OpKind.PUSH_INT:
                if not isinstance(self.value, int):
                    raise TypeError(f"`{self.kind}` requires value of type `int`")
            # Requires `Intrinsic`
            case OpKind.PRINT:
                if not isinstance(self.value, Intrinsic):
                    raise TypeError(f"`{self.kind}` requires value of type `Intrinsic`")
            # Requires `Operator`
            case OpKind.ADD:
                if not isinstance(self.value, Operator):
                    raise TypeError(f"`{self.kind}` requires value of type `Operator`")
            case _:
                assert_never(self.kind)
