# Parser Library

Import `parser` for cursor-based text scanning:

```casa
import "parser"
```

`Cursor` contains the source string and a mutable `u64` position. `ParseError`
contains a message and the `u64` position at which parsing failed.

## Cursor API

| Method | Result or action |
|---|---|
| `Cursor::new source:str -> Cursor` | Cursor at position `0` |
| `is_eof self:Cursor -> bool` | Whether the position reached the end |
| `peek self:Cursor -> Option[char]` | Current character without advancing |
| `peek_at self:Cursor offset:u64 -> Option[char]` | Character at a relative offset |
| `advance self:Cursor -> Option[char]` | Current character, then advance |
| `starts_with self:Cursor prefix:str -> bool` | Match remaining text without advancing |
| `expect_char self:Cursor expected:char -> Result[char ParseError]` | Consume one expected character |
| `skip self:Cursor count:u64` | Advance by a count |
| `take_string self:Cursor target:str -> Result[str ParseError]` | Consume exact text |
| `skip_while self:Cursor predicate:fn[char -> bool]` | Advance while matching |
| `take_while self:Cursor predicate:fn[char -> bool] -> str` | Consume and return matching text |
| `save self:Cursor -> u64` | Current position |
| `restore self:Cursor saved:u64` | Return to a saved position |

```casa
"name=42" Cursor::new = cursor
&char::is_alpha cursor.take_while print    # name
'=' cursor.expect_char drop
cursor parse_int .unwrap print           # 42
```

## Ready-made parsers

| Function | Result |
|---|---|
| `skip_whitespace cursor:Cursor` | Skip ASCII whitespace |
| `parse_int cursor:Cursor -> Result[i64 ParseError]` | Signed decimal integer |
| `parse_identifier cursor:Cursor -> Result[str ParseError]` | Casa-style identifier |
| `parse_escape cursor:Cursor -> Result[char ParseError]` | Character after a backslash |
| `parse_quoted_string cursor:Cursor -> Result[str ParseError]` | Double-quoted text |
| `parse_char_literal cursor:Cursor -> Result[char ParseError]` | Single-quoted character |

The library also exports `str_to_int`, `is_ident_start`, and `is_ident_char` for
custom parsers.

Use `save` and `restore` when alternatives need backtracking. The ready-made
integer and quoted-literal parsers restore their starting position on failure.

See [`examples/parser.casa`](../examples/parser.casa) for a runnable parser.
