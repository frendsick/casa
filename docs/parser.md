# Parser Library

Import `parser` for cursor-based text scanning:

```casa
import "parser"
```

`Cursor` [borrows](ownership.md#borrow-for-a-call) the source string and contains a mutable `u64` position. `ParseError`
contains a message and the `u64` position at which parsing failed.

Positions and offsets count bytes. `peek`, `peek_at`, and `advance` convert one
byte to a `char`. They do not decode multibyte UTF-8 characters. Use this cursor
for ASCII syntax unless the caller handles UTF-8 decoding separately.

Reference tables abbreviate library type names and list inputs in consumption
order. Source examples use qualified names. See [reference notation](notation.md)
for signatures, fragments, and commands for running complete examples.

## Cursor API

| Method | Signature | Description |
|---|---|---|
| [advance](#cursoradvance) | `fn advance self:mut$Cursor -> std::Option[char]` | Current character, then advance |
| [expect_char](#cursorexpect_char) | `fn expect_char self:mut$Cursor expected:char -> std::Result[char ParseError]` | Consume one expected character |
| [is_eof](#cursoris_eof) | `fn is_eof self:$Cursor -> bool` | Whether the position reached the end |
| [new](#cursornew) | `fn new source:$str -> Cursor` | Cursor at position `0` |
| [peek](#cursorpeek) | `fn peek self:$Cursor -> std::Option[char]` | Current character without advancing |
| [peek_at](#cursorpeek_at) | `fn peek_at self:$Cursor offset:u64 -> std::Option[char]` | Character at a relative offset |
| [restore](#cursorrestore) | `fn restore self:mut$Cursor saved:u64` | Return to a saved position |
| [save](#cursorsave) | `fn save self:$Cursor -> u64` | Current position |
| [skip](#cursorskip) | `fn skip self:mut$Cursor count:u64` | Advance by a count |
| [skip_while](#cursorskip_while) | `fn skip_while self:mut$Cursor pred:fn[char -> bool]` | Advance while matching |
| [starts_with](#cursorstarts_with) | `fn starts_with self:$Cursor prefix:$str -> bool` | Match remaining text without advancing |
| [take_string](#cursortake_string) | `fn take_string self:mut$Cursor target:$str -> std::Result[str ParseError]` | Consume exact text |
| [take_while](#cursortake_while) | `fn take_while self:mut$Cursor pred:fn[char -> bool] -> std::String` | Consume and copy matching text |

### Cursor::advance

```text
fn advance self:mut$Cursor -> std::Option[char]
```

Returns the current character and advances the cursor.

### Cursor::expect_char

```text
fn expect_char self:mut$Cursor expected:char -> std::Result[char ParseError]
```

Consumes one expected character.

### Cursor::is_eof

```text
fn is_eof self:$Cursor -> bool
```

Returns whether the position reached the end.

### Cursor::new

```text
fn new source:$str -> Cursor
```

Creates a cursor at position `0` over the borrowed source.

### Cursor::peek

```text
fn peek self:$Cursor -> std::Option[char]
```

Returns the current character without advancing the cursor.

### Cursor::peek_at

```text
fn peek_at self:$Cursor offset:u64 -> std::Option[char]
```

Returns the character at a relative offset without advancing the cursor.

### Cursor::restore

```text
fn restore self:mut$Cursor saved:u64
```

Returns to a saved position.

### Cursor::save

```text
fn save self:$Cursor -> u64
```

Returns the current cursor position.

### Cursor::skip

```text
fn skip self:mut$Cursor count:u64
```

Advances by a count.

### Cursor::skip_while

```text
fn skip_while self:mut$Cursor pred:fn[char -> bool]
```

Advances while matching.

### Cursor::starts_with

```text
fn starts_with self:$Cursor prefix:$str -> bool
```

Matches remaining text without advancing.

### Cursor::take_string

```text
fn take_string self:mut$Cursor target:$str -> std::Result[str ParseError]
```

Consumes exact text.

### Cursor::take_while

```text
fn take_while self:mut$Cursor pred:fn[char -> bool] -> std::String
```

Consumes and copies matching text.

### Cursor example

```casa
import "std"
import "parser"

"name=42" parser::Cursor::new = cursor
&char::is_alpha cursor.take_while print # name
'=' cursor.expect_char.unwrap drop
cursor parser::parse_int.unwrap print # 42
```

## Ready-made parsers

| Function | Result |
|---|---|
| `parse_char_literal cursor:mut$Cursor -> Result[char ParseError]` | Single-quoted character |
| `parse_escape cursor:mut$Cursor -> Result[char ParseError]` | Character after a backslash |
| `parse_identifier cursor:mut$Cursor -> Result[String ParseError]` | ASCII letter or underscore, followed by ASCII letters, digits, or underscores |
| `parse_int cursor:mut$Cursor -> Result[i64 ParseError]` | Signed decimal integer |
| `parse_quoted_string cursor:mut$Cursor -> Result[String ParseError]` | Double-quoted text |
| `skip_whitespace cursor:mut$Cursor` | Skip ASCII whitespace |

The library also exports `str_to_int`, `is_ident_start`, and `is_ident_char` for
custom parsers.

Use `save` and `restore` when alternatives need backtracking. The ready-made
integer and quoted-literal parsers restore their starting position on failure.

`parse_escape` accepts `\n`, `\t`, `\r`, `\0`, `\\`, `\"`, `\'`, `\{`, and
`\}`. It does not implement the compiler's `\xNN` and `\u{...}` escapes.

`parse_int` accepts an optional minus sign followed by decimal digits in the
complete `i64` range. It stops before the first non-digit. Missing digits and
overflow return `ParseError` at the starting position and restore the cursor.

`str_to_int` is a wrapping conversion for callers that already validated decimal
syntax. It does not check range. The compiler uses it to retain integer-literal
bits, including values above the `i64` maximum that can fit `u64`. Use
`str.to_int` for a checked complete token or `parse_int` for a checked cursor
prefix.

See [examples/parser.casa](../examples/parser.casa) for a runnable parser.
