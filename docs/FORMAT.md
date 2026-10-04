# Casa Format Guide

`casafmt` reads Casa source from standard input and writes formatted source to
standard output.

Formatting uses the root syntax product from `compiler/products.casa`. Its
lossless grammar in `compiler/syntax.casa` retains source spelling,
trivia, qualified-name ranges, and nested constructs without loading imports or
looking up declarations. Unresolved names and both meanings of `Name {}` are
safe to format. Field labels and assignment annotations are recognized from
their grammatical position.

The formatter checks tokens, comment attachment, and structural relationships
against the formatted candidate. If recognition fails or these facts change,
it returns the original source and a failure status. Analysis and assembly
use the source builder to resolve declarations and check structured bodies.

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

Code fragments use declarations and bindings from their surrounding example.
Names such as `std::List` assume `import "std"`. See
[reference notation](notation.md#library-names-and-examples).

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
- When a function signature exceeds 100 characters, use the wrapping form (see below).

---

## Blank lines

- **1 blank line** before top-level definitions (`fn`, `struct`, `enum`,
  `impl`, `trait`, with an optional `pub` prefix).
- **1 blank line** before and after import groups.
- Consecutive plain top-level statements (root bindings, assignments, map `.set` chains)
  are grouped **without** blank lines.
- Consecutive `import` statements are grouped **without** blank lines.
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
    fn new -> Foo { 0 0 Foo }
}
std::Map[str i64]::new = MY_MAP
1 "a" MY_MAP.set
2 "b" MY_MAP.set

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
std::List[T]::new = values
value std::List[T]::from_array
```

---

## Comments

- Always write one space between `#` and the comment text: `# text` not `#text`.
- Preserve comment text and attachment.
- Keep a trailing comment on the line of the structural unit it follows.
- Keep a standalone comment on its own line at the indentation of the unit it
  describes.
- Keep standalone comments directly above the code they describe, with no blank
  line between them. Separate a top-level comment block from preceding code
  with one blank line.
- Omit comments that only repeat a nearby name or operation.
- Put a `# SAFETY:` comment immediately before the [unsafe](functions-and-lambdas.md#unsafe-boundaries) block or `unsafe fn`
  that it justifies. Do not add `# SAFETY:` comments in test files.
- Use a plain comment for a section label. Do not surround it with decorative
  separator lines.

```casa
# Escape sequence map
```

---

## Array and list literals

- Items MUST be comma-separated, with a space after each comma. A missing comma
  between items is a syntax error (see
  [ADR-0154](adr/0154-array-literals-require-commas-between-items.md)).
- A single trailing comma before `]` is allowed. The compact form omits it. The
  expanded form adds it.
- One space before the opening `[` when it follows another token:

```casa
# Correct
["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
[1, 2, 3] std::List::from_array = nums
["hello", "Casa"] = greetings

# Wrong
["0","1","2"]
[1,2,3]std::List::from_array = nums
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

Put no space between an enum variant's name and its payload type list:

```casa
enum Shape {
    Circle(i64)
    Rectangle(i64 i64)
    Point
}
```

Outside an enum declaration, put no space between a variant's qualified name
and its data parentheses:

```casa
# Correct
OpValue::FnCall(value)
std::Option::Some(x)
Type::Generic(generic)

# Wrong
OpValue::FnCall (value)
std::Option::Some (x)
Type::Generic (generic)
```

This applies to pattern matching (`is` checks), constructors, and `match` arms.

---

## Constant annotations

Put one space after the colon in a constant annotation:

```casa
const BUFFER_BYTES: u64 { ELEMENT_COUNT ELEMENT_BYTES * }
```

Keep type arguments adjacent to a function reference: `&length[BUFFER_BYTES]`.

## Binding annotations

Put one space after the colon in a binding annotation. Function parameters use
the compact `name:type` form.

```casa
255 = byte: u8
std::Option::None = absent: std::Option[i64]
```

---

## Struct and enum field layout

- Struct and enum fields use `name: Type` or `pub name: Type` (space after colon).
- When a struct has 2 or more fields, **align type names to the same column**:

```casa
struct Parser {
    sources:        SourceStore
    included_files: Set[str]
}

struct Token {
    pub kind: TokenKind
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

## Function signatures

### Single-line form

When the function signature fits within the line-length limit, write everything on one line.
Parameters use `name:type` (no space after colon):

```casa
fn fizzbuzz number:i64 {
    ...
}

fn add a:i64 b:i64 -> i64 { a b + }

extern fn strlen text:$cstr -> u64
```

Put one space before a function's type-parameter list. Type bounds use a
compact colon:

```casa
fn identity [T] value:T -> T { value }

fn duplicate [T:Clone] value:$T -> T { value.clone }
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

When the function signature would exceed 100 characters, wrap as follows.
Keep `pub`, [unsafe](functions-and-lambdas.md#unsafe-boundaries), or `extern` before `fn` on the first line.

- `fn name` alone on the first line
- Each parameter on its own line, indented 4 spaces, `name:type` compact
- `-> ReturnType {` on its own line at column 0

```casa
fn make_compiler_with_tables
    sources:SourceStore
    ops:std::List[Op]
    function:std::Option[Function]
    string_table:std::List[str]
    constants_table:std::List[str]
-> SourceReader {
    ...
}
```

Multiple return types follow the same pattern:

```casa
fn split_pair
    input:str
    delimiter:str
    trim_whitespace:bool
    preserve_empty_fields:bool
    include_delimiter:bool
-> str str {
    ...
}
```

An `unsafe` function prefixes the declaration with `unsafe`. The same rule
applies to the wrapped form, whose first line is `unsafe fn name`:

```casa
unsafe fn read_word address:ptr -> u64 {
    unsafe { address load64 }
}
```

An extern function prefixes the bodyless declaration with `extern`. A wrapped
extern declaration ends after its return type and has no opening brace:

```casa
extern fn native_operation
    destination_buffer:ptr
    destination_buffer_size:u64
    source_buffer:ptr
    source_buffer_size:u64
-> i32
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

Use `String` for incremental or loop-based string construction:

```casa
std::String::new = text
while items.is_empty ! do
    items.pop.as_str text.append
done
text
```

Never use `str::concat` for more than two strings.
