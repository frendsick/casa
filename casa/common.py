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
    LOAD8 = auto()
    LOAD16 = auto()
    LOAD32 = auto()
    LOAD64 = auto()
    STORE8 = auto()
    STORE16 = auto()
    STORE32 = auto()
    STORE64 = auto()
    # IO
    PRINT = auto()

    # Functions
    EXEC = auto()

    # Syscalls
    SYSCALL0 = auto()
    SYSCALL1 = auto()
    SYSCALL2 = auto()
    SYSCALL3 = auto()
    SYSCALL4 = auto()
    SYSCALL5 = auto()
    SYSCALL6 = auto()

    @classmethod
    def from_lowercase(cls, value: str) -> Self | None:
        if not value.islower():
            return None
        return cls.__members__.get(value.upper())


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
    TRAIT = auto()

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

    # Bitwise
    BIT_AND = auto()
    BIT_OR = auto()
    BIT_XOR = auto()
    BIT_NOT = auto()

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
    # Bitwise
    "&": Operator.BIT_AND,
    "|": Operator.BIT_OR,
    "^": Operator.BIT_XOR,
    "~": Operator.BIT_NOT,
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
    PRINT_BOOL = auto()
    PRINT_CHAR = auto()
    PRINT_CSTR = auto()
    PRINT_INT = auto()
    PRINT_STR = auto()

    # Memory
    HEAP_ALLOC = auto()
    LOAD8 = auto()
    LOAD16 = auto()
    LOAD32 = auto()
    LOAD64 = auto()
    STORE8 = auto()
    STORE16 = auto()
    STORE32 = auto()
    STORE64 = auto()

    # Syscalls
    SYSCALL0 = auto()
    SYSCALL1 = auto()
    SYSCALL2 = auto()
    SYSCALL3 = auto()
    SYSCALL4 = auto()
    SYSCALL5 = auto()
    SYSCALL6 = auto()

    # Literals
    PUSH_ARRAY = auto()
    PUSH_BOOL = auto()
    PUSH_CHAR = auto()
    PUSH_INT = auto()
    PUSH_NONE = auto()
    PUSH_STR = auto()
    SOME = auto()

    # Arithmetic
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    MOD = auto()

    # Bitshift
    SHL = auto()
    SHR = auto()

    # Bitwise
    BIT_AND = auto()
    BIT_OR = auto()
    BIT_XOR = auto()
    BIT_NOT = auto()

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
    type_annotation: str | None = None

    def __post_init__(self):
        assert len(OpKind) == 79, "Exhaustive handling for `OpKind`"

        match self.kind:
            # Requires Python `None`
            case OpKind.PUSH_NONE | OpKind.SOME:
                if self.value is not None:
                    raise TypeError(f"`{self.kind}` requires value of type `NoneType`")
            # Requires `bool`
            case OpKind.PUSH_BOOL:
                if not isinstance(self.value, bool):
                    raise TypeError(f"`{self.kind}` requires value of type `bool`")
            # Requires `int`
            case OpKind.FSTRING_CONCAT | OpKind.PUSH_CHAR | OpKind.PUSH_INT:
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
                OpKind.DROP
                | OpKind.DUP
                | OpKind.FN_EXEC
                | OpKind.HEAP_ALLOC
                | OpKind.LOAD8
                | OpKind.LOAD16
                | OpKind.LOAD32
                | OpKind.LOAD64
                | OpKind.OVER
                | OpKind.PRINT
                | OpKind.PRINT_BOOL
                | OpKind.PRINT_CHAR
                | OpKind.PRINT_CSTR
                | OpKind.PRINT_INT
                | OpKind.PRINT_STR
                | OpKind.ROT
                | OpKind.STORE8
                | OpKind.STORE16
                | OpKind.STORE32
                | OpKind.STORE64
                | OpKind.SWAP
                | OpKind.SYSCALL0
                | OpKind.SYSCALL1
                | OpKind.SYSCALL2
                | OpKind.SYSCALL3
                | OpKind.SYSCALL4
                | OpKind.SYSCALL5
                | OpKind.SYSCALL6
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
                | OpKind.BIT_AND
                | OpKind.BIT_NOT
                | OpKind.BIT_OR
                | OpKind.BIT_XOR
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
    PUSH_CHAR = auto()
    PUSH_STR = auto()
    ROT = auto()
    SWAP = auto()

    # IO
    PRINT_BOOL = auto()
    PRINT_CHAR = auto()
    PRINT_CSTR = auto()
    PRINT_INT = auto()
    PRINT_STR = auto()

    # Memory
    HEAP_ALLOC = auto()
    LOAD8 = auto()
    LOAD16 = auto()
    LOAD32 = auto()
    LOAD64 = auto()
    STORE8 = auto()
    STORE16 = auto()
    STORE32 = auto()
    STORE64 = auto()

    # Arithmetic
    ADD = auto()
    DIV = auto()
    MOD = auto()
    MUL = auto()
    SUB = auto()

    # Bitshift
    SHL = auto()
    SHR = auto()

    # Bitwise
    BIT_AND = auto()
    BIT_OR = auto()
    BIT_XOR = auto()
    BIT_NOT = auto()

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

    # Syscalls
    SYSCALL0 = auto()
    SYSCALL1 = auto()
    SYSCALL2 = auto()
    SYSCALL3 = auto()
    SYSCALL4 = auto()
    SYSCALL5 = auto()
    SYSCALL6 = auto()


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
        assert len(InstKind) == 66, "Exhaustive handling for `InstructionKind`"

        match self.kind:
            # Should not have a parameter
            case (
                InstKind.ADD
                | InstKind.AND
                | InstKind.BIT_AND
                | InstKind.BIT_NOT
                | InstKind.BIT_OR
                | InstKind.BIT_XOR
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
                | InstKind.LOAD8
                | InstKind.LOAD16
                | InstKind.LOAD32
                | InstKind.LOAD64
                | InstKind.LT
                | InstKind.MOD
                | InstKind.MUL
                | InstKind.NE
                | InstKind.NOT
                | InstKind.OR
                | InstKind.OVER
                | InstKind.PRINT_BOOL
                | InstKind.PRINT_CHAR
                | InstKind.PRINT_CSTR
                | InstKind.PRINT_INT
                | InstKind.PRINT_STR
                | InstKind.ROT
                | InstKind.SHL
                | InstKind.SHR
                | InstKind.STORE8
                | InstKind.STORE16
                | InstKind.STORE32
                | InstKind.STORE64
                | InstKind.SUB
                | InstKind.SWAP
                | InstKind.SYSCALL0
                | InstKind.SYSCALL1
                | InstKind.SYSCALL2
                | InstKind.SYSCALL3
                | InstKind.SYSCALL4
                | InstKind.SYSCALL5
                | InstKind.SYSCALL6
            ):
                if self.args:
                    raise TypeError(
                        f"`{self.kind}` should not have any parameters\nArguments: {self.args}"
                    )
            # One parameter of type `int`
            case (
                InstKind.FSTRING_CONCAT
                | InstKind.GLOBALS_INIT
                | InstKind.GLOBAL_GET
                | InstKind.GLOBAL_SET
                | InstKind.JUMP
                | InstKind.JUMP_NE
                | InstKind.LABEL
                | InstKind.LOCALS_INIT
                | InstKind.LOCALS_UNINIT
                | InstKind.LOCAL_GET
                | InstKind.LOCAL_SET
                | InstKind.PUSH
                | InstKind.PUSH_CHAR
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
BUILTIN_TYPES: set[str] = {
    "int",
    "bool",
    "char",
    "cstr",
    "str",
    "ptr",
    "array",
    "any",
    "option",
}


def extract_generic_base(typ: str) -> str | None:
    """Extract the base name from a parameterized generic type.

    For 'array[int]' returns 'array', for 'List[int]' returns 'List'.
    Returns None for fn types and non-parameterized types.
    """
    bracket = typ.find("[")
    if bracket <= 0 or not typ.endswith("]"):
        return None
    base = typ[:bracket]
    if base == "fn":
        return None
    return base


def extract_generic_inner(typ: str) -> str | None:
    """Extract the inner type from a parameterized generic type.

    For 'array[int]' returns 'int', for 'List[str]' returns 'str'.
    Returns None for fn types and non-parameterized types.
    """
    base = extract_generic_base(typ)
    if base is None:
        return None
    return typ[len(base) + 1 : -1]


def split_type_tokens(s: str) -> list[str]:
    """Split a space-separated type string into tokens, respecting brackets.

    For 'str int' returns ['str', 'int'].
    For 'str List[int]' returns ['str', 'List[int]'].
    For 'int -> int' returns ['int', '->', 'int'].
    """
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


def extract_generic_params(typ: str) -> list[str] | None:
    """Extract multiple generic type parameters respecting bracket depth.

    For 'Map[str int]' returns ['str', 'int'].
    For 'Map[str List[int]]' returns ['str', 'List[int]'].
    For 'array[int]' returns ['int'].
    Returns None for fn types and non-parameterized types.
    """
    base = extract_generic_base(typ)
    if base is None:
        return None
    inner = typ[len(base) + 1 : -1]
    return split_type_tokens(inner)


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
    trait_bounds: dict[str, str] = field(default_factory=dict)

    @classmethod
    def from_str(cls, repr: str) -> Self:
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

        tokens = split_type_tokens(repr)
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
                    extract_generic_base(b.typ) == a.typ
                    or extract_generic_base(a.typ) == b.typ
                ):
                    return False

        for ra, rb in zip(self.return_types, other.return_types, strict=True):
            if ra != rb and ra != ANY_TYPE and rb != ANY_TYPE:
                if not (
                    extract_generic_base(rb) == ra or extract_generic_base(ra) == rb
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
class TraitMethod:
    name: str
    signature: Signature


@dataclass
class Trait:
    name: str
    methods: list[TraitMethod]
    location: Location


TRAIT_SELF_TYPE = "self"


def _replace_trait_self(typ: str, replacement: str) -> str:
    """Replace the trait self placeholder in a type string."""
    if typ == TRAIT_SELF_TYPE:
        return replacement
    return typ


def resolve_trait_sig(sig: "Signature", type_var: str) -> "Signature":
    """Create a copy of a trait method signature with self type replaced by type_var.

    Only replaces self in type positions, not in parameter names,
    since 'self' is a common parameter name.
    """
    params = []
    for p in sig.parameters:
        resolved_typ = _replace_trait_self(p.typ, type_var)
        params.append(Parameter(resolved_typ, p.name))
    ret = [_replace_trait_self(r, type_var) for r in sig.return_types]
    return Signature(params, ret)


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
    variables: list[Variable] = field(default_factory=list)
    captures: list[Variable] = field(default_factory=list)


GLOBAL_FUNCTIONS: OrderedDict[str, Function] = OrderedDict()
GLOBAL_STRUCTS: OrderedDict[str, Struct] = OrderedDict()
GLOBAL_TRAITS: OrderedDict[str, Trait] = OrderedDict()
GLOBAL_VARIABLES: OrderedDict[str, Variable] = OrderedDict()
GLOBAL_SCOPE_LABEL = "_start"
INCLUDED_FILES: set[Path] = set()


@dataclass
class CompilationContext:
    """Holds all mutable state for a single compilation run."""

    functions: OrderedDict[str, Function] = field(default_factory=OrderedDict)
    structs: OrderedDict[str, Struct] = field(default_factory=OrderedDict)
    traits: OrderedDict[str, Trait] = field(default_factory=OrderedDict)
    variables: OrderedDict[str, Variable] = field(default_factory=OrderedDict)
    included_files: set[Path] = field(default_factory=set)


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
