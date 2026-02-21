from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Any, Generic, OrderedDict, Self, Sequence, TypeVar, assert_never

T = TypeVar("T")


class TokenKind(Enum):
    DELIMITER = auto()
    EOF = auto()
    FSTRING_END = auto()
    FSTRING_EXPR_END = auto()
    FSTRING_EXPR_START = auto()
    FSTRING_START = auto()
    FSTRING_TEXT = auto()
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
    ALLOC = auto()
    LOAD = auto()
    STORE = auto()

    # IO
    PRINT = auto()

    # Functions
    EXEC = auto()

    # Ownership
    CLONE = auto()
    FREE = auto()

    @classmethod
    def from_lowercase(cls, value: str) -> Self | None:
        if not value.islower():
            return None
        return cls.__members__.get(value.upper())


class OwnershipState(Enum):
    OWNED = auto()
    MOVED = auto()
    COPY = auto()


class Keyword(Enum):
    # Functions
    FN = auto()
    IMPL = auto()
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

    # Data types
    STRUCT = auto()

    # Include files
    INCLUDE = auto()

    @classmethod
    def from_lowercase(cls, value: str) -> Self | None:
        if not value.islower():
            return None
        return cls.__members__.get(value.upper())


class Delimiter(Enum):
    ARROW = auto()
    COMMA = auto()
    COLON = auto()
    DOT = auto()
    HASHTAG = auto()
    OPEN_BRACE = auto()
    CLOSE_BRACE = auto()
    OPEN_BRACKET = auto()
    CLOSE_BRACKET = auto()
    OPEN_PAREN = auto()
    CLOSE_PAREN = auto()

    @classmethod
    def from_str(cls, value: str) -> Self | None:
        return cls._MAPPING.get(value)  # type: ignore


Delimiter._MAPPING = {  # type: ignore
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
assert len(Delimiter._MAPPING) == len(Delimiter), "Exhaustive handling for `Delimiter`"  # type: ignore


class Operator(Enum):
    # Arithmetic
    PLUS = auto()
    MINUS = auto()
    MULTIPLICATION = auto()
    DIVISION = auto()
    MODULO = auto()

    # Bitshift
    SHL = auto()
    SHR = auto()

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
        return cls._MAPPING.get(value)  # type: ignore


Operator._MAPPING = {  # type: ignore
    # Arithmetic
    "+": Operator.PLUS,
    "-": Operator.MINUS,
    "*": Operator.MULTIPLICATION,
    "/": Operator.DIVISION,
    "%": Operator.MODULO,
    # Bitshift
    "<<": Operator.SHL,
    ">>": Operator.SHR,
    # Boolean
    "&&": Operator.AND,
    "||": Operator.OR,
    "!": Operator.NOT,
    # Comparison
    "==": Operator.EQ,
    ">=": Operator.GE,
    ">": Operator.GT,
    "<=": Operator.LE,
    "<": Operator.LT,
    "!=": Operator.NE,
    # Assignment
    "=": Operator.ASSIGN,
    "-=": Operator.ASSIGN_DECREMENT,
    "+=": Operator.ASSIGN_INCREMENT,
}
assert len(Operator._MAPPING) == len(Operator), "Exhaustive handling for `Operator`"  # type: ignore


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
    # Stack
    DROP = auto()
    DUP = auto()
    OVER = auto()
    ROT = auto()
    SWAP = auto()

    # IO
    PRINT = auto()
    PRINT_INT = auto()
    PRINT_STR = auto()

    # Memory
    HEAP_ALLOC = auto()
    HEAP_FREE = auto()
    LOAD = auto()
    STORE = auto()

    # Literals
    PUSH_ARRAY = auto()
    PUSH_BOOL = auto()
    PUSH_INT = auto()
    PUSH_STR = auto()

    # Arithmetic
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    MOD = auto()

    # Bitshift
    SHL = auto()
    SHR = auto()

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
    METHOD_CALL = auto()

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
    PUSH_CAPTURE = auto()
    PUSH_VARIABLE = auto()

    # Structs
    STRUCT_NEW = auto()

    # Types
    TYPE_CAST = auto()

    # Ownership
    CLONE = auto()

    # Include files
    INCLUDE_FILE = auto()

    # F-strings
    FSTRING_CONCAT = auto()

    # Identifiers should be resolved by the parser
    IDENTIFIER = auto()


@dataclass
class Op:
    value: Any
    kind: OpKind
    location: Location
    typ: str | None = None

    def __post_init__(self):
        assert len(OpKind) == 58, "Exhaustive handling for `OpKind`"

        match self.kind:
            # Requires `bool`
            case OpKind.PUSH_BOOL:
                if not isinstance(self.value, bool):
                    raise TypeError(f"`{self.kind}` requires value of type `bool`")
            # Requires `int`
            case OpKind.FSTRING_CONCAT | OpKind.PUSH_INT:
                if not isinstance(self.value, int):
                    raise TypeError(f"`{self.kind}` requires value of type `int`")
            # Requires `str`
            case (
                OpKind.ASSIGN_DECREMENT
                | OpKind.ASSIGN_INCREMENT
                | OpKind.ASSIGN_VARIABLE
                | OpKind.FN_CALL
                | OpKind.FN_PUSH
                | OpKind.IDENTIFIER
                | OpKind.METHOD_CALL
                | OpKind.PUSH_CAPTURE
                | OpKind.PUSH_STR
                | OpKind.PUSH_VARIABLE
                | OpKind.TYPE_CAST
            ):
                if not isinstance(self.value, str):
                    raise TypeError(f"`{self.kind}` requires value of type `str`")
            # Requires `list`
            case OpKind.PUSH_ARRAY:
                if not isinstance(self.value, list):
                    raise TypeError(f"`{self.kind}` requires value of type `list`")
            # Requires `Intrinsic`
            case (
                OpKind.CLONE
                | OpKind.DROP
                | OpKind.DUP
                | OpKind.FN_EXEC
                | OpKind.HEAP_ALLOC
                | OpKind.HEAP_FREE
                | OpKind.LOAD
                | OpKind.OVER
                | OpKind.PRINT
                | OpKind.PRINT_INT
                | OpKind.PRINT_STR
                | OpKind.ROT
                | OpKind.STORE
                | OpKind.SWAP
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
                | OpKind.SHL
                | OpKind.SHR
            ):
                if not isinstance(self.value, Operator):
                    raise TypeError(f"`{self.kind}` requires value of type `Operator`")
            # Requires `Path`
            case OpKind.INCLUDE_FILE:
                if not isinstance(self.value, Path):
                    raise TypeError(f"`{self.kind}` requires value of type `Path`")
            # Requires `Struct`
            case OpKind.STRUCT_NEW:
                if not isinstance(self.value, Struct):
                    raise TypeError(f"`{self.kind}` requires value of type `Struct`")
            case _:
                assert_never(self.kind)


class InstKind(Enum):
    # Stack
    DROP = auto()
    DUP = auto()
    OVER = auto()
    PUSH = auto()
    PUSH_STR = auto()
    ROT = auto()
    SWAP = auto()

    # IO
    PRINT_INT = auto()
    PRINT_STR = auto()

    # Memory
    HEAP_ALLOC = auto()
    HEAP_FREE = auto()
    HEAP_FREE_ARRAY = auto()
    CLONE_ARRAY = auto()
    CLONE_STRUCT = auto()
    LOAD = auto()
    STORE = auto()

    # Arithmetic
    ADD = auto()
    DIV = auto()
    MOD = auto()
    MUL = auto()
    SUB = auto()

    # Bitshift
    SHL = auto()
    SHR = auto()

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

    # Constants
    CONSTANT_LOAD = auto()
    CONSTANT_STORE = auto()

    # F-strings
    FSTRING_CONCAT = auto()


@dataclass
class Inst:
    kind: InstKind
    args: list = field(default_factory=list)
    location: Location | None = None

    @property
    def int_arg(self) -> int:
        """Single int argument accessor."""
        assert len(self.args) == 1, f"Expected 1 argument, got {len(self.args)}"
        assert isinstance(self.args[0], int), f"Expected int, got {type(self.args[0])}"
        return self.args[0]

    @property
    def str_arg(self) -> str:
        """Single str argument accessor."""
        assert len(self.args) == 1, f"Expected 1 argument, got {len(self.args)}"
        assert isinstance(self.args[0], str), f"Expected str, got {type(self.args[0])}"
        return self.args[0]

    def __post_init__(self):
        assert len(InstKind) == 49, "Exhaustive handling for `InstructionKind`"

        match self.kind:
            # Should not have a parameter
            case (
                InstKind.ADD
                | InstKind.AND
                | InstKind.CLONE_ARRAY
                | InstKind.DIV
                | InstKind.DROP
                | InstKind.DUP
                | InstKind.EQ
                | InstKind.FN_EXEC
                | InstKind.FN_RETURN
                | InstKind.GE
                | InstKind.GT
                | InstKind.HEAP_ALLOC
                | InstKind.LE
                | InstKind.LOAD
                | InstKind.LT
                | InstKind.MOD
                | InstKind.MUL
                | InstKind.NE
                | InstKind.NOT
                | InstKind.OR
                | InstKind.OVER
                | InstKind.PRINT_INT
                | InstKind.PRINT_STR
                | InstKind.ROT
                | InstKind.SHL
                | InstKind.SHR
                | InstKind.STORE
                | InstKind.SUB
                | InstKind.SWAP
            ):
                if self.args:
                    raise TypeError(
                        f"`{self.kind}` should not have any parameters\nArguments: {self.args}"
                    )
            # One parameter of type `int`
            case (
                InstKind.CLONE_STRUCT
                | InstKind.FSTRING_CONCAT
                | InstKind.GLOBALS_INIT
                | InstKind.GLOBAL_GET
                | InstKind.GLOBAL_SET
                | InstKind.HEAP_FREE
                | InstKind.HEAP_FREE_ARRAY
                | InstKind.JUMP
                | InstKind.JUMP_NE
                | InstKind.LABEL
                | InstKind.LOCALS_INIT
                | InstKind.LOCALS_UNINIT
                | InstKind.LOCAL_GET
                | InstKind.LOCAL_SET
                | InstKind.PUSH
                | InstKind.PUSH_STR
                | InstKind.CONSTANT_LOAD
                | InstKind.CONSTANT_STORE
            ):
                if len(self.args) != 1 or not isinstance(self.args[0], int):
                    raise TypeError(
                        f"`{self.kind}` requires one parameter of type `int`\nArguments: {self.args}"
                    )
            # One parameter of type `str`
            case InstKind.FN_CALL | InstKind.FN_PUSH:
                if len(self.args) != 1 or not isinstance(self.args[0], str):
                    raise TypeError(
                        f"`{self.kind}` requires one parameter of type `str`\nArguments: {self.args}"
                    )


Bytecode = list[Inst]
LabelId = int
Type = str


@dataclass
class Program:
    bytecode: Bytecode  # Global scope instructions
    functions: dict[str, Bytecode]  # Function name -> bytecode
    strings: list[str]  # String table (index = string ID)
    globals_count: int  # Number of global variables
    constants_count: int  # Number of constant/capture slots


ANY_TYPE = "any"
BUILTIN_TYPES: set[str] = {"int", "bool", "str", "ptr", "array", "any"}


def is_array_type(typ: str) -> bool:
    """Check if a type is an array type (bare or parameterized)."""
    return typ == "array" or typ.startswith("array[")


def extract_array_element_type(typ: str) -> str | None:
    """Extract the element type from a parameterized array type.

    Returns None for bare 'array' or non-array types.
    """
    array_prefix = "array["
    if not typ.startswith(array_prefix) or not typ.endswith("]"):
        return None
    return typ[len(array_prefix) : -1]


def is_fn_type(typ: str) -> bool:
    """Check if a type is a function type (bare or parameterized)."""
    return typ == "fn" or typ.startswith("fn[")


def extract_fn_signature_str(typ: str) -> str | None:
    """Extract the signature string from a parameterized fn type.

    Returns None for bare 'fn' or non-fn types.
    """
    fn_prefix = "fn["
    if not typ.startswith(fn_prefix) or not typ.endswith("]"):
        return None
    return typ[len(fn_prefix) : -1]


def is_owned_type(typ: str) -> bool:
    """Check if a type has ownership semantics (heap-allocated)."""
    if typ in ("int", "bool", "str", "ptr", "any") or is_fn_type(typ):
        return False
    if is_array_type(typ) or typ in GLOBAL_STRUCTS:
        return True
    return False


@dataclass
class Parameter:
    typ: Type
    name: str | None = None

    def __repr__(self) -> str:
        if self.name:
            return f"{self.name}:{self.typ}"
        return self.typ


@dataclass
class Signature:
    parameters: list[Parameter]
    return_types: list[Type]
    type_vars: set[str] = field(default_factory=set)

    @classmethod
    def from_str(cls, repr: str) -> Self:
        def tokenize(s: str) -> list[str]:
            """Split a signature string into type tokens, respecting brackets."""
            tokens: list[str] = []
            current: list[str] = []
            depth = 0
            for ch in s.strip():
                if ch == "[":
                    depth += 1
                    current.append(ch)
                elif ch == "]":
                    depth -= 1
                    current.append(ch)
                elif ch == " " and depth == 0:
                    if current:
                        tokens.append("".join(current))
                        current = []
                else:
                    current.append(ch)
            if current:
                tokens.append("".join(current))
            return tokens

        def split_on_arrow(tokens: list[str]) -> tuple[list[str], list[str]]:
            """Split token list on '->' at bracket depth 0."""
            for i, tok in enumerate(tokens):
                if tok == "->":
                    return tokens[:i], tokens[i + 1 :]
            raise ValueError(f"Invalid signature: {repr}")

        def parse_type_list(tokens: list[str]) -> list[Type]:
            if len(tokens) == 1 and tokens[0] == "None":
                return []
            return list(tokens)

        tokens = tokenize(repr)
        param_tokens, return_tokens = split_on_arrow(tokens)
        parameters = [Parameter(p) for p in parse_type_list(param_tokens)]
        return_types = parse_type_list(return_tokens)
        return cls(parameters, return_types)

    def __repr__(self):
        parameters = " ".join(p.typ for p in self.parameters) or "None"
        return_types = " ".join(t for t in self.return_types) or "None"
        return f"{parameters} -> {return_types}"

    def matches(self, other: Self) -> bool:
        if len(self.parameters) != len(other.parameters) or len(
            self.return_types
        ) != len(other.return_types):
            return False

        for a, b in zip(self.parameters, other.parameters, strict=True):
            if a.typ != b.typ and a.typ != ANY_TYPE and b.typ != ANY_TYPE:
                if not (
                    (a.typ == "array" and is_array_type(b.typ))
                    or (b.typ == "array" and is_array_type(a.typ))
                ):
                    return False

        for ra, rb in zip(self.return_types, other.return_types, strict=True):
            if ra != rb and ra != ANY_TYPE and rb != ANY_TYPE:
                if not (
                    (ra == "array" and is_array_type(rb))
                    or (rb == "array" and is_array_type(ra))
                ):
                    return False

        return True


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
class Member:
    name: str
    typ: Type

    def __hash__(self) -> int:
        return hash(self.name)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Variable):
            return self.name == other.name
        if isinstance(other, str):
            return self.name == other
        return False


@dataclass
class Struct:
    name: str
    members: list[Member]
    location: Location


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
    is_typechecked: bool = False
    is_auto_generated: bool = False
    variables: list[Variable] = field(default_factory=list)
    captures: list[Variable] = field(default_factory=list)


GLOBAL_FUNCTIONS: OrderedDict[str, Function] = OrderedDict()
GLOBAL_STRUCTS: OrderedDict[str, Struct] = OrderedDict()
GLOBAL_VARIABLES: OrderedDict[str, Variable] = OrderedDict()
GLOBAL_SCOPE_LABEL = "_start"
INCLUDED_FILES: set[Path] = set()


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
