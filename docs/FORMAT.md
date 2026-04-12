# Casa Format Guide

Mechanical formatting rules for Casa source files enforced by `casafmt`.
All rules are MUST unless noted otherwise.

See [STYLE.md](./STYLE.md) for naming conventions and idiomatic patterns.

---

## Indentation

- Use **4 spaces** per indentation level.
- Never use tabs.

```casa
fn fizzbuzz number:int {
    number 3 % 0 == = fizz
    if fizz then
        "Fizz\n" print
    fi
}
```

---

## Line length

- Lines SHOULD NOT exceed **100 characters**.
- String literals in examples and expected-output lines are exempt.
- When a function signature exceeds 100 characters, use the wrapping form (see below).

---

## Blank lines

- **1 blank line** before and after top-level definitions (`fn`, `struct`, `enum`,
  `impl`, `trait`) and include groups.
- Consecutive plain top-level statements (global assignments, map `.set` chains)
  are grouped **without** blank lines.
- Consecutive `include` statements are grouped **without** blank lines.
- **2 blank lines** immediately before a section separator comment.
- Inside a function body, no more than **1 consecutive blank line** (SHOULD).

```casa
include "../lib/std.casa"
include "../lib/io.casa"

16 = BUFFER_SIZE

struct Foo {
    x: int
    y: int
}

impl Foo {
    fn new -> Foo {
        0 0 Foo
    }
}

Map::new (Map[str int]) = MY_MAP
"a" 1 MY_MAP.set = MY_MAP
"b" 2 MY_MAP.set = MY_MAP

fn bar {
    # first group
    1 = a

    # second group
    2 = b
}
```

---

## Trailing whitespace

Trailing spaces or tabs at the end of a line are forbidden.

---

## Comments

- Always write one space between `#` and the comment text: `# text` not `#text`.
- Section separator comments may use either `=` or `-` repeated characters — author's
  choice. Both are acceptable; do not mix styles within a file.

```casa
# ============================================================================
# Section using = style
# ============================================================================

# ---------------------------------------------------------------------------
# Section using - style
# ---------------------------------------------------------------------------
```

---

## Struct and enum field layout

- Struct and enum fields use `name: Type` (space after colon).
- When a struct has 2 or more fields, **align type names to the same column**:

```casa
struct Parser {
    store:          SymbolStore
    included_files: Set[str]
}

struct Token {
    kind:     TokenKind
    location: Location
    value:    str
}
```

- One field per line. Never put multiple fields on the same line.

---

## Function signatures

### Single-line form

When the signature fits within the line-length limit, write everything on one line.
Parameters use `name:type` (no space after colon):

```casa
fn fizzbuzz number:int {
    ...
}

fn add a:int b:int -> int {
    a b +
}
```

### Wrapped form

When the signature would exceed 100 characters, wrap as follows:

- `fn name` alone on the first line
- Each parameter on its own line, indented 4 spaces, `name:type` compact
- `-> ReturnType {` on its own line at column 0

```casa
fn make_compiler_with_tables
    store:SymbolStore
    ops:List[Op]
    function:int
    string_table:List[str]
    constants_table:List[str]
-> BytecodeCompiler {
    ...
}
```

Multiple return types follow the same pattern:

```casa
fn split_pair
    input:str
    delimiter:str
-> str str {
    ...
}
```

---

## Getter chaining and method pipelines

- Write getters directly against their receiver with **no space**: `struct.field`,
  `list.length`, `token.location.file`.
- Getter chains always stay on **one line**, regardless of depth:

```casa
error.location.span.length
token.location.file
```

- When chaining **three or more method calls** whose results feed into each other,
  put each method on its own line, indented 4 spaces from the receiver:

```casa
value
    .step_one
    .step_two
    .step_three
```

---

## `if` / `elif` / `else` / `fi`

**Statements** — always split across lines. `if`, `then`, body, and `fi` each on their
own lines:

```casa
if fizz then
    "Fizz\n" print
elif buzz then
    "Buzz\n" print
else
    number print
fi
```

**Value expressions** — single-line form is allowed when the entire `if` is used as an
inline value (e.g. assigned to a variable or returned):

```casa
if b then "true" else "false" fi = result
```

**Multiple conditions** — each condition on its own line, with the boolean operator
at the end of the condition line it connects to:

```casa
if
    cond1
    cond2 &&
    cond3 ||
then
    ...
fi
```

---

## `while` / `do` / `done`

Always split across lines:

```casa
while index size > do
    # body
    1 += index
done
```

---

## `match` / `end` arms

- Single statement or expression: **single-line arm**
- Two or more statements: **block arm** with `{ }`

```casa
color match
    Color::Red => "red" print
    Color::Green => "green" print
    Color::Blue => "blue" print
end

shape match
    Shape::Circle(radius) => {
        "radius=" print
        radius print
        "\n" print
    }
    Shape::Point => "point\n" print
end
```

---

## f-strings vs string concatenation

Prefer f-strings whenever embedding one or more values into a string literal:

```casa
# Preferred
f"Hello, {name}!" print

# Avoid: str::concat for 3+ strings
name " is " str::concat age int::to_str str::concat print
```

Use `StringBuilder` for incremental or loop-based string construction:

```casa
StringBuilder::new = builder
items.length 0 == ! while
    items.pop builder.append
done
builder.build
```

Never use `str::concat` for more than two strings.
