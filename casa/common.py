from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Any, Generic, Self, Sequence, TypeVar, assert_never

T = TypeVar("T")


class TokenKind(Enum):
    DELIMITER = auto()
    EOF = auto()
    IDENTIFIER = auto()
    INTRINSIC = auto()
    KEYWORD = auto()
    LITERAL = auto()
    OPERATOR = auto()


class Intrinsic(Enum):
    # Stack
    DROP = auto()
    DUP = auto()
    OVER = auto()
    ROT = auto()
    SWAP = auto()

    # Memory
    LOAD = auto()
    STORE = auto()

    # IO
    PRINT = auto()

    @classmethod
    def from_lowercase(cls, value: str) -> Self | None:
        if not value.islower():
            return None
        return cls.__members__.get(value.upper())


class Keyword(Enum):
    FN = auto()

    @classmethod
    def from_lowercase(cls, value: str) -> Self | None:
        if not value.islower():
            return None
        return cls.__members__.get(value.upper())


class Delimiter(Enum):
    COMMA = auto()
    OPEN_BRACE = auto()
    CLOSE_BRACE = auto()
    OPEN_BRACKET = auto()
    CLOSE_BRACKET = auto()

    @classmethod
    def from_str(cls, value: str) -> Self | None:
        mapping = {
            ",": cls.COMMA,
            "{": cls.OPEN_BRACE,
            "}": cls.CLOSE_BRACE,
            "[": cls.OPEN_BRACKET,
            "]": cls.CLOSE_BRACKET,
        }
        assert len(mapping) == len(Delimiter), "Exhaustive handling for `Delimiter`"
        return mapping.get(value)  # type: ignore


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
    # Intrinsics
    DROP = auto()
    DUP = auto()
    OVER = auto()
    ROT = auto()
    SWAP = auto()
    LOAD = auto()
    PRINT = auto()
    STORE = auto()

    # Literals
    PUSH_INT = auto()
    PUSH_LIST = auto()

    # Operators
    ADD = auto()

    # Functions
    CALL_FN = auto()

    # Identifiers
    IDENTIFIER = auto()


@dataclass
class Op:
    value: Any
    kind: OpKind
    location: Location

    def __post_init__(self):
        assert len(OpKind) == 13, "Exhaustive handling for `OpKind`"

        match self.kind:
            # Requires `int`
            case OpKind.PUSH_INT:
                if not isinstance(self.value, int):
                    raise TypeError(f"`{self.kind}` requires value of type `int`")
            # Requires `str`
            case OpKind.IDENTIFIER | OpKind.CALL_FN:
                if not isinstance(self.value, str):
                    raise TypeError(f"`{self.kind}` requires value of type `str`")
            # Requires `list`
            case OpKind.PUSH_LIST:
                if not isinstance(self.value, list):
                    raise TypeError(f"`{self.kind}` requires value of type `list`")
            # Requires `Intrinsic`
            case (
                OpKind.DROP
                | OpKind.DUP
                | OpKind.OVER
                | OpKind.ROT
                | OpKind.SWAP
                | OpKind.LOAD
                | OpKind.PRINT
                | OpKind.STORE
            ):
                if not isinstance(self.value, Intrinsic):
                    raise TypeError(f"`{self.kind}` requires value of type `Intrinsic`")
            # Requires `Operator`
            case OpKind.ADD:
                if not isinstance(self.value, Operator):
                    raise TypeError(f"`{self.kind}` requires value of type `Operator`")
            case _:
                assert_never(self.kind)


class InstructionKind(Enum):
    # Stack
    DROP = auto()
    DUP = auto()
    OVER = auto()
    PUSH = auto()
    ROT = auto()
    SWAP = auto()

    # Intrinsics
    LOAD = auto()
    PRINT = auto()
    STORE = auto()

    # Lists
    LIST_NEW = auto()

    # Operators
    ADD = auto()

    # Functions
    CALL_FN = auto()


@dataclass
class Instruction:
    kind: InstructionKind
    arguments: list = field(default_factory=list)

    def __post_init__(self):
        assert len(InstructionKind) == 12, "Exhaustive handling for `InstructionKind`"

        match self.kind:
            # Should not have a parameter
            case (
                InstructionKind.ADD
                | InstructionKind.DROP
                | InstructionKind.DUP
                | InstructionKind.LIST_NEW
                | InstructionKind.LOAD
                | InstructionKind.OVER
                | InstructionKind.PRINT
                | InstructionKind.ROT
                | InstructionKind.STORE
                | InstructionKind.SWAP
            ):
                if self.arguments:
                    raise TypeError(
                        f"`{self.kind}` should not have any parameters\nArguments: {self.arguments}"
                    )
            # One parameter of type `int`
            case InstructionKind.PUSH:
                if len(self.arguments) != 1 or not isinstance(self.arguments[0], int):
                    raise TypeError(
                        f"`{self.kind}` requires one parameter of type `int`\nArguments: {self.arguments}"
                    )
            # One parameter of type `str`
            case InstructionKind.CALL_FN:
                if len(self.arguments) != 1 or not isinstance(self.arguments[0], str):
                    raise TypeError(
                        f"`{self.kind}` requires one parameter of type `str`\nArguments: {self.arguments}"
                    )


@dataclass
class Function:
    name: str
    ops: list[Op]
    location: Location


GLOBAL_IDENTIFIERS: dict[str, Function] = {}


type Bytecode = list[Instruction]


@dataclass
class Cursor(Generic[T]):
    sequence: Sequence[T]
    position: int = 0

    def is_finished(self) -> bool:
        return self.position >= len(self.sequence)

    def peek(self) -> T | None:
        if self.is_finished():
            return None
        return self.sequence[self.position]

    def pop(self) -> T | None:
        x = self.peek()
        if x is not None:
            self.position += 1
        return x
