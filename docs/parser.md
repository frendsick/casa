# Parser Library

Import `parser` for cursor-based text scanning:

```casa
import "parser"
```

`Cursor` borrows the source string and contains a mutable `u64` position. `ParseError`
contains a message and the `u64` position at which parsing failed.

Reference tables abbreviate library type names and list inputs in consumption
order. Source examples use qualified names. See [reference notation](notation.md)
for signatures, fragments, and commands for running complete examples.

## Cursor API

| Method | Signature | Behavior |
|---|---|---|
| [Cursor::new](#cursornew) | `fn new source:$str -> Cursor` | Cursor at position `0` |
| [Cursor::is_eof](#cursoris_eof) | `fn is_eof self:$Cursor -> bool` | Whether the position reached the end |
| [Cursor::peek](#cursorpeek) | `fn peek self:$Cursor -> std::Option[char]` | Current character without advancing |
| [Cursor::peek_at](#cursorpeek_at) | `fn peek_at self:$Cursor offset:u64 -> std::Option[char]` | Character at a relative offset |
| [Cursor::advance](#cursoradvance) | `fn advance self:mut$Cursor -> std::Option[char]` | Current character, then advance |
| [Cursor::starts_with](#cursorstarts_with) | `fn starts_with self:$Cursor prefix:$str -> bool` | Match remaining text without advancing |
| [Cursor::expect_char](#cursorexpect_char) | `fn expect_char self:mut$Cursor expected:char -> std::Result[char ParseError]` | Consume one expected character |
| [Cursor::skip](#cursorskip) | `fn skip self:mut$Cursor count:u64` | Advance by a count |
| [Cursor::take_string](#cursortake_string) | `fn take_string self:mut$Cursor target:$str -> std::Result[str ParseError]` | Consume exact text |
| [Cursor::skip_while](#cursorskip_while) | `fn skip_while self:mut$Cursor pred:fn[char -> bool]` | Advance while matching |
| [Cursor::take_while](#cursortake_while) | `fn take_while self:mut$Cursor pred:fn[char -> bool] -> std::String` | Consume and copy matching text |
| [Cursor::save](#cursorsave) | `fn save self:$Cursor -> u64` | Current position |
| [Cursor::restore](#cursorrestore) | `fn restore self:mut$Cursor saved:u64` | Return to a saved position |

### Cursor::new

Creates a cursor at position `0` over the borrowed source.

### Cursor::is_eof

Returns whether the position reached the end.

### Cursor::peek

Returns the current character without advancing the cursor.

### Cursor::peek_at

Returns the character at a relative offset without advancing the cursor.

### Cursor::advance

Returns the current character and advances the cursor.

### Cursor::starts_with

Matches remaining text without advancing.

### Cursor::expect_char

Consumes one expected character.

### Cursor::skip

Advances by a count.

### Cursor::take_string

Consumes exact text.

### Cursor::skip_while

Advances while matching.

### Cursor::take_while

Consumes and copies matching text.

### Cursor::save

Returns the current cursor position.

### Cursor::restore

Returns to a saved position.


```casa
import "std"
import "parser"

"name=42" parser::Cursor::new = cursor
&char::is_alpha cursor.take_while print    # name
'=' cursor.expect_char.unwrap drop
cursor parser::parse_int .unwrap print           # 42
```

## Ready-made parsers

| Function | Result |
|---|---|
| `skip_whitespace cursor:mut$Cursor` | Skip ASCII whitespace |
| `parse_int cursor:mut$Cursor -> Result[i64 ParseError]` | Signed decimal integer |
| `parse_identifier cursor:mut$Cursor -> Result[String ParseError]` | Casa-style identifier |
| `parse_escape cursor:mut$Cursor -> Result[char ParseError]` | Character after a backslash |
| `parse_quoted_string cursor:mut$Cursor -> Result[String ParseError]` | Double-quoted text |
| `parse_char_literal cursor:mut$Cursor -> Result[char ParseError]` | Single-quoted character |

The library also exports `str_to_int`, `is_ident_start`, and `is_ident_char` for
custom parsers.

Use `save` and `restore` when alternatives need backtracking. The ready-made
integer and quoted-literal parsers restore their starting position on failure.

See [`examples/parser.casa`](../examples/parser.casa) for a runnable parser.
