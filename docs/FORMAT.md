# Casa Format Guide

`casafmt` reads Casa source from standard input and writes formatted source to
standard output.

Build a current compiler first, then build the formatter:

```sh
./casac casa.casa -o casac-next -L lib
./casac-next formatter/format.casa -o casafmt -L lib
```

Write to a temporary file so a formatter failure cannot replace the source:

```sh
./casafmt < program.casa > program.casa.tmp && mv program.casa.tmp program.casa
```

`casafmt` validates input and output with the compiler syntax parser. This check
does not load imports, resolve identifiers, or typecheck. On a lexical, syntax,
output-validation, or source-preservation error, `casafmt` writes the original
source unchanged, reports the error on standard error, and exits with status
`1`. The command above leaves `program.casa` unchanged.

The formatter accepts LF, CRLF, and bare CR line endings. Successful output uses
LF and ends with exactly one newline.

The remaining sections define the mechanical rules that `casafmt` enforces.
All rules are MUST unless noted otherwise.

See [STYLE.md](./STYLE.md) for naming conventions and idiomatic patterns.

---

## Indentation

- Use **4 spaces** per indentation level.
- Never use tabs.

```casa
fn fizzbuzz number:i64 {
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
- When a function declaration exceeds 100 characters, use the wrapping form (see below).

---

## Blank lines

- **1 blank line** before and after top-level definitions (`fn`, `struct`, `enum`,
  `impl`, `trait`) and import groups.
- Consecutive plain top-level statements (global assignments, map `.set` chains)
  are grouped **without** blank lines.
- Consecutive `import` statements are grouped **without** blank lines.
- **1 blank line** immediately before a section separator comment.
- Inside a freeform composition, preserve at most **1 author-supplied blank
  line**.

```casa
import "std"
import "os"

16 = BUFFER_SIZE

struct Foo {
    x: i64
    y: i64
}

impl Foo {
    fn new -> Foo {
        0 0 Foo
    }
}

Map[str i64]::new = MY_MAP
1 "a" MY_MAP.set = MY_MAP
2 "b" MY_MAP.set = MY_MAP

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

## Qualified calls

Do not put whitespace around `::` in qualified calls or references:

```casa
List[T]::new = values
value List[T]::from_array
```

---

## Comments

- Always write one space between `#` and the comment text: `# text` not `#text`.
- Preserve comment text and attachment.
- Keep a trailing comment on the line of the structural unit it follows.
- Keep a standalone comment on its own line at the indentation of the unit it
  describes.
- Section separator comments may use either `=` or `-` repeated characters.
  Choose one style and do not mix styles within a file.

```casa
# ============================================================================
# Section using = style
# ============================================================================

# ---------------------------------------------------------------------------
# Section using - style
# ---------------------------------------------------------------------------
```

---

## Array and list literals

- Items MUST be comma-separated, with a space after each comma. A missing comma
  between items is a syntax error (see
  [ADR-0153](adr/0153-array-literals-require-commas-between-items.md)).
- A single trailing comma before `]` is allowed. The compact form omits it; the
  expanded form adds it.
- One space before the opening `[` when it follows another token:

```casa
# Correct
["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
[1, 2, 3] List::from_array = nums
"hello" [0, 5] str::slice

# Wrong
["0","1","2"]
[1,2,3]List::from_array = nums
[1 2 3]              # missing commas: syntax error
```

- A delimited form has one canonical layout regardless of how the source
  line-breaks its items or whether it carries a trailing comma.
- Keep the form **compact on one line** when the canonical line fits within 100
  characters.
- When it does not fit, **expand it**: put one item on each indented line, add a
  **trailing comma** after the last item, align the closing `]` with the column
  of the opener, and keep any composition suffix on the closing `]` line.

```casa
# Fits within 100 characters: compact.
[1, 2, 3] sum

# Exceeds 100 characters: expand, trailing comma, aligned `]`, suffix on `]`.
[
    11111111,
    22222222,
    33333333,
    44444444,
    55555555,
    66666666,
    77777777,
    88888888,
    99999999,
    10101010,
] values
```

---

## Enum variant data parentheses

No space between an enum variant name and its data parentheses:

```casa
# Correct
OpValue::FnCall(value)
Option::Some(x)
Type::Generic(generic)

# Wrong
OpValue::FnCall (value)
Option::Some (x)
Type::Generic (generic)
```

This applies to pattern matching (`is` checks), constructors, and `match` arms.

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

- One field or variant per line, regardless of how the source groups them.
  Never put multiple fields or variants on the same line.

```casa
# Source may group variants; the formatter splits them.
enum Color { Red Green Blue }
```

```casa
enum Color {
    Red
    Green
    Blue
}
```

---

## Function declarations

### Single-line form

When the function declaration fits within the line-length limit, write everything on one line.
Parameters use `name:type` (no space after colon):

```casa
fn fizzbuzz number:i64 {
    ...
}

fn add a:i64 b:i64 -> i64 {
    a b +
}
```

### Inline definitions

A top-level `fn`, method, or trait-default definition is joined onto **one
line** only when all of these hold:

- The complete one-line form fits within 100 characters.
- The declaration does not wrap.
- The body has at most one nonblank composition line, with no comment, no nested
  block, and no delimited form.

Write an empty body as `{ }`. The formatter joins an eligible definition even
when the source splits its braces across lines:

```casa
fn add a:i64 b:i64 -> i64 { a b + }

fn noop { }
```

Any definition that is not eligible uses a **multiline body**. This rule does
not apply to lambdas or match-arm blocks.

### Wrapped form

When the function declaration would exceed 100 characters, wrap as follows:

- `fn name` alone on the first line
- Each parameter on its own line, indented 4 spaces, `name:type` compact
- `-> ReturnType {` on its own line at column 0

```casa
fn make_compiler_with_tables
    store:SymbolStore
    ops:List[Op]
    function:Option[Function]
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

An unsafe function prefixes the declaration with `unsafe`. The same rule
applies to the wrapped form, whose first line is `unsafe fn name`:

```casa
unsafe fn read_word address:ptr -> u64 {
    unsafe { address load64 }
}
```

---

## Getter chaining and method pipelines

- Write getters directly against their receiver with **no space**: `struct.field`,
  `list.length`, `token.location.file`.
- Keep one or two accessor calls on one line:

```casa
analysis.result.document
```

- Put every accessor call in a chain of three or more on its own continuation
  line. This syntax-only rule applies equally to field getters and method calls,
  regardless of the chain's consumer:

```casa
analysis
    .result
    .document
    .location
```

```casa
value
    .step_one
    .step_two
    .step_three
```

---

## Freeform compositions

The formatter preserves each author-supplied nonblank line boundary outside a
syntax-directed structure. It does not join or wrap arbitrary operation
sequences to meet the 100-character target. It preserves at most one supplied
blank line between those composition lines.

Structural rules can add or remove line boundaries for definitions,
declarations, delimiters, fields, control forms, match arms, getter chains, and
method pipelines.

---

## `if` / `elif` / `else` / `fi`

Statement forms use multiline bodies. Keep a short condition with `then` on the
opening line:

```casa
if fizz then
    "Fizz\n" print
elif buzz then
    "Buzz\n" print
else
    number print
fi
```

A value-producing form stays inline only when it is part of a larger preserved
source line and the complete normalized line fits within 100 characters:

```casa
if b then "true" else "false" fi = result
```

A condition that already has multiple nonblank composition lines keeps those
lines. Put `if` and `then` on separate lines, and indent each condition line:

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

Use a multiline body. Keep a short condition with `do` on the opening line:

```casa
while index size > do
    # body
    1 += index
done
```

A condition with multiple preserved composition lines uses the same layout as
the multiline `if` condition. Put `while` and `do` on separate lines.

---

## `for` / `in` / `do` / `done`

Use a multiline body and keep the iterator expression with `do` when it fits:

```casa
for value in values.iter do
    value print
done
```

---

## `match` / `end` arms

- Put `match` arms on indented lines and align `end` with the matched value.
- Keep a single operation or expression on the arm line.
- Expand a block arm with its braces and body on separate lines.

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
name " is " str::concat age i64::to_str str::concat print
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
