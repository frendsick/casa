# Parser Library

Import `parser` for cursor-based text scanning:

```casa
import "parser"
```

`Cursor` borrows the source string and contains a mutable `u64` position. `ParseError`
contains a message and the `u64` position at which parsing failed.

## Cursor API

| Method | Result or action |
|---|---|
| `Cursor::new source:$str -> Cursor` | Cursor at position `0` |
| `is_eof self:$Cursor -> bool` | Whether the position reached the end |
| `peek self:$Cursor -> Option[char]` | Current character without advancing |
| `peek_at self:$Cursor offset:u64 -> Option[char]` | Character at a relative offset |
| `advance self:mut$Cursor -> Option[char]` | Current character, then advance |
| `starts_with self:$Cursor prefix:$str -> bool` | Match remaining text without advancing |
| `expect_char self:mut$Cursor expected:char -> Result[char ParseError]` | Consume one expected character |
| `skip self:mut$Cursor count:u64` | Advance by a count |
| `take_string self:mut$Cursor target:$str -> Result[str ParseError]` | Consume exact text |
| `skip_while self:mut$Cursor predicate:fn[char -> bool]` | Advance while matching |
| `take_while self:mut$Cursor predicate:fn[char -> bool] -> String` | Consume and copy matching text |
| `save self:$Cursor -> u64` | Current position |
| `restore self:mut$Cursor saved:u64` | Return to a saved position |

```casa
"name=42" Cursor::new = cursor
&char::is_alpha cursor.take_while print    # name
'=' cursor.expect_char drop
cursor parse_int .unwrap print           # 42
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
