from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Any, Generic, OrderedDict, Self, Sequence, TypeVar, assert_never

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

    # Functions
    EXEC = auto()

    @classmethod
    def from_lowercase(cls, value: str) -> Self | None:
        if not value.islower():
            return None
        return cls.__members__.get(value.upper())


class Keyword(Enum):
    # Functions
    FN = auto()
    RETURN = auto()

    # Loops
    WHILE = auto()
    DO = auto()
    BREAK = auto()
    CONTINUE = auto()
    DONE = auto()

    # Conditionals
    IF = auto()
    THEN = auto()
    ELIF = auto()
    ELSE = auto()
    FI = auto()

    @classmethod
    def from_lowercase(cls, value: str) -> Self | None:
        if not value.islower():
            return None
        return cls.__members__.get(value.upper())


class Delimiter(Enum):
    COMMA = auto()
    COLON = auto()
    OPEN_BRACE = auto()
    CLOSE_BRACE = auto()
    OPEN_BRACKET = auto()
    CLOSE_BRACKET = auto()

    @classmethod
    def from_str(cls, value: str) -> Self | None:
        mapping = {
            ",": cls.COMMA,
            ":": cls.COLON,
            "{": cls.OPEN_BRACE,
            "}": cls.CLOSE_BRACE,
            "[": cls.OPEN_BRACKET,
            "]": cls.CLOSE_BRACKET,
        }
        assert len(mapping) == len(Delimiter), "Exhaustive handling for `Delimiter`"
        return mapping.get(value)  # type: ignore


class Operator(Enum):
    # Arithmetic
    PLUS = auto()
    MINUS = auto()
    MULTIPLICATION = auto()
    DIVISION = auto()
    MODULO = auto()

    # Boolean
    AND = auto()
    OR = auto()
    NOT = auto()

    # Comparison
    EQ = auto()
    GE = auto()
    GT = auto()
    LE = auto()
    LT = auto()
    NE = auto()

    # Assignment
    ASSIGN = auto()
    ASSIGN_DECREMENT = auto()
    ASSIGN_INCREMENT = auto()

    @classmethod
    def from_str(cls, value: str) -> Self | None:
        mapping = {
            # Arithmetic
            "+": cls.PLUS,
            "-": cls.MINUS,
            "*": cls.MULTIPLICATION,
            "/": cls.DIVISION,
            "%": cls.MODULO,
            # Boolean
            "&&": cls.AND,
            "||": cls.OR,
            "!": cls.NOT,
            # Comparison
            "==": cls.EQ,
            ">=": cls.GE,
            ">": cls.GT,
            "<=": cls.LE,
            "<": cls.LT,
            "!=": cls.NE,
            # Assignment
            "=": cls.ASSIGN,
            "-=": cls.ASSIGN_DECREMENT,
            "+=": cls.ASSIGN_INCREMENT,
        }
        assert len(mapping) == len(Operator), "Exhaustive handling for `Operator`"
        return mapping.get(value)  # type: ignore


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
    PUSH_STR = auto()

    # Arithmetic
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    MOD = auto()

    # Boolean
    AND = auto()
    OR = auto()
    NOT = auto()

    # Comparison
    EQ = auto()
    GE = auto()
    GT = auto()
    LE = auto()
    LT = auto()
    NE = auto()

    # Functions
    FN_CALL = auto()
    FN_EXEC = auto()
    FN_PUSH = auto()
    FN_RETURN = auto()

    # Loops
    WHILE_START = auto()
    WHILE_CONDITION = auto()
    WHILE_BREAK = auto()
    WHILE_CONTINUE = auto()
    WHILE_END = auto()

    # Conditionals
    IF_START = auto()
    IF_CONDITION = auto()
    IF_ELIF = auto()
    IF_ELSE = auto()
    IF_END = auto()

    # Variables
    ASSIGN_DECREMENT = auto()
    ASSIGN_INCREMENT = auto()
    ASSIGN_VARIABLE = auto()
    PUSH_VARIABLE = auto()

    # Identifiers should be resolved by the parser
    IDENTIFIER = auto()


@dataclass
class Op:
    value: Any
    kind: OpKind
    location: Location

    def __post_init__(self):
        assert len(OpKind) == 44, "Exhaustive handling for `OpKind`"

        match self.kind:
            # Requires `int`
            case OpKind.PUSH_INT:
                if not isinstance(self.value, int):
                    raise TypeError(f"`{self.kind}` requires value of type `int`")
            # Requires `str`
            case (
                OpKind.IDENTIFIER
                | OpKind.FN_CALL
                | OpKind.FN_PUSH
                | OpKind.PUSH_STR
                | OpKind.PUSH_VARIABLE
                | OpKind.ASSIGN_DECREMENT
                | OpKind.ASSIGN_INCREMENT
                | OpKind.ASSIGN_VARIABLE
            ):
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
                | OpKind.FN_EXEC
            ):
                if not isinstance(self.value, Intrinsic):
                    raise TypeError(f"`{self.kind}` requires value of type `Intrinsic`")
            # Requires `Keyword`
            case (
                OpKind.FN_RETURN
                | OpKind.IF_START
                | OpKind.IF_CONDITION
                | OpKind.IF_ELIF
                | OpKind.IF_ELSE
                | OpKind.IF_END
                | OpKind.WHILE_START
                | OpKind.WHILE_CONDITION
                | OpKind.WHILE_BREAK
                | OpKind.WHILE_CONTINUE
                | OpKind.WHILE_END
            ):
                if not isinstance(self.value, Keyword):
                    raise TypeError(f"`{self.kind}` requires value of type `Keyword`")
            # Requires `Operator`
            case (
                OpKind.ADD
                | OpKind.AND
                | OpKind.DIV
                | OpKind.MOD
                | OpKind.MUL
                | OpKind.SUB
                | OpKind.EQ
                | OpKind.GE
                | OpKind.GT
                | OpKind.LE
                | OpKind.LT
                | OpKind.NE
                | OpKind.NOT
                | OpKind.OR
            ):
                if not isinstance(self.value, Operator):
                    raise TypeError(f"`{self.kind}` requires value of type `Operator`")
            case _:
                assert_never(self.kind)


class InstKind(Enum):
    # Stack
    DROP = auto()
    DUP = auto()
    OVER = auto()
    PUSH = auto()
    PUSH_STR = auto()  # TODO: Implement with `PUSH`
    ROT = auto()
    SWAP = auto()

    # Intrinsics
    LOAD = auto()
    PRINT = auto()
    STORE = auto()

    # Lists
    LIST_NEW = auto()

    # Arithmetic
    ADD = auto()
    DIV = auto()
    MOD = auto()
    MUL = auto()
    SUB = auto()

    # Boolean
    AND = auto()
    OR = auto()
    NOT = auto()

    # Comparison
    EQ = auto()
    GE = auto()
    GT = auto()
    LE = auto()
    LT = auto()
    NE = auto()

    # Functions
    FN_CALL = auto()
    FN_EXEC = auto()
    FN_RETURN = auto()

    # Jumps
    LABEL = auto()
    JUMP = auto()
    JUMP_NE = auto()

    # Locals
    LOCALS_INIT = auto()
    LOCALS_UNINIT = auto()
    LOCAL_GET = auto()
    LOCAL_SET = auto()

    # Globals
    GLOBALS_INIT = auto()
    GLOBAL_GET = auto()
    GLOBAL_SET = auto()


@dataclass
class Inst:
    kind: InstKind
    arguments: list = field(default_factory=list)

    def __post_init__(self):
        assert len(InstKind) == 38, "Exhaustive handling for `InstructionKind`"

        match self.kind:
            # Should not have a parameter
            case (
                InstKind.ADD
                | InstKind.AND
                | InstKind.DIV
                | InstKind.DROP
                | InstKind.DUP
                | InstKind.EQ
                | InstKind.FN_EXEC
                | InstKind.FN_RETURN
                | InstKind.GE
                | InstKind.GT
                | InstKind.LE
                | InstKind.LIST_NEW
                | InstKind.LOAD
                | InstKind.LT
                | InstKind.MOD
                | InstKind.MUL
                | InstKind.NE
                | InstKind.NOT
                | InstKind.OR
                | InstKind.OVER
                | InstKind.PRINT
                | InstKind.ROT
                | InstKind.STORE
                | InstKind.SUB
                | InstKind.SWAP
            ):
                if self.arguments:
                    raise TypeError(
                        f"`{self.kind}` should not have any parameters\nArguments: {self.arguments}"
                    )
            # One parameter of type `int`
            case (
                InstKind.GLOBAL_GET
                | InstKind.GLOBAL_SET
                | InstKind.GLOBALS_INIT
                | InstKind.JUMP
                | InstKind.JUMP_NE
                | InstKind.LABEL
                | InstKind.LOCALS_INIT
                | InstKind.LOCALS_UNINIT
                | InstKind.LOCAL_GET
                | InstKind.LOCAL_SET
                | InstKind.PUSH
            ):
                if len(self.arguments) != 1 or not isinstance(self.arguments[0], int):
                    raise TypeError(
                        f"`{self.kind}` requires one parameter of type `int`\nArguments: {self.arguments}"
                    )
            # One parameter of type `str`
            case InstKind.FN_CALL | InstKind.PUSH_STR:
                if len(self.arguments) != 1 or not isinstance(self.arguments[0], str):
                    raise TypeError(
                        f"`{self.kind}` requires one parameter of type `str`\nArguments: {self.arguments}"
                    )


Bytecode = list[Inst]
LabelId = int
Type = str


@dataclass
class Signature:
    parameters: list[Type]
    return_types: list[Type]

    @classmethod
    def from_str(cls, repr: str) -> Self:
        def parse_type_list(part: str) -> list[Type]:
            if part.strip() == "None":
                return []

            types: list[Type] = []
            for token in part.split():
                types.append(token)
            return types

        if "->" not in repr:
            raise ValueError(f"Invalid signature: {repr}")

        param_part, return_part = repr.split("->", 1)
        parameters = parse_type_list(param_part)
        return_types = parse_type_list(return_part)
        return cls(parameters, return_types)

    def __repr__(self):
        parameters = " ".join(t for t in self.parameters) or "None"
        return_types = " ".join(t for t in self.return_types) or "None"
        return f"{parameters} -> {return_types}"


@dataclass
class Variable:
    name: str
    typ: Type | None = None  # Resolved during type checking

    def __hash__(self) -> int:
        return hash(self.name)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Variable):
            return self.name == other.name
        if isinstance(other, str):
            return self.name == other
        return False


@dataclass
class Function:
    name: str
    ops: list[Op]
    location: Location
    # Missing signature will be inferred during type checking
    signature: Signature | None = None
    # Bytecode will be compiled if the function is used
    bytecode: Bytecode | None = None
    is_used: bool = False
    variables: list[Variable] = field(default_factory=list)


GLOBAL_FUNCTIONS: OrderedDict[str, Function] = OrderedDict()
GLOBAL_VARIABLES: OrderedDict[str, Variable] = OrderedDict()
GLOBAL_SCOPE_LABEL = "_start"


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
