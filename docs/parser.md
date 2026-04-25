# Parser Library

The parser library provides cursor-based text scanning primitives for building parsers in Casa. It is in `lib/parser.casa`. Import it with:

```casa
import "path/to/lib/parser.casa"
```

The parser library includes the standard library (`lib/std.casa`) automatically.

## Structs

### `Cursor`

A cursor tracks a position within a source string. All scanning methods operate on a shared cursor, advancing its position as characters are consumed.

```casa
struct Cursor {
    source: str
    pos:    int
}
```

### `ParseError`

Returned by parsers when input does not match the expected pattern. Contains a human-readable message and the position where the error occurred.

```casa
struct ParseError {
    message: str
    pos:     int
}
```

## Cursor Methods

### `Cursor::new`

Creates a cursor at position 0.

**Signature:** `Cursor::new source:str -> Cursor`

**Stack effect:** `str -> Cursor`

```casa
"hello world" Cursor::new = cursor
```

### `Cursor::is_eof`

Returns `true` if the cursor is at or past the end of the source string.

**Signature:** `Cursor::is_eof self:Cursor -> bool`

**Stack effect:** `Cursor -> bool`

```casa
"" Cursor::new .is_eof print    # true
```

### `Cursor::peek`

Returns the current character without advancing the cursor. Returns `Option::None` at EOF.

**Signature:** `Cursor::peek self:Cursor -> Option[char]`

**Stack effect:** `Cursor -> Option[char]`

```casa
"hello" Cursor::new .peek .unwrap print    # h
```

### `Cursor::peek_at`

Returns the character at `pos + offset` without advancing the cursor. Returns `Option::None` if out of bounds.

**Signature:** `Cursor::peek_at self:Cursor offset:int -> Option[char]`

**Stack effect:** `Cursor int -> Option[char]`

```casa
2 "hello" Cursor::new .peek_at .unwrap print    # l
```

### `Cursor::advance`

Returns the current character and advances the cursor by one. Returns `Option::None` at EOF.

**Signature:** `Cursor::advance self:Cursor -> Option[char]`

**Stack effect:** `Cursor -> Option[char]`

```casa
"hello" Cursor::new = cursor
cursor.advance .unwrap print    # h
cursor.advance .unwrap print    # e
```

### `Cursor::starts_with`

Checks if the remaining input (from the current position) starts with the given prefix.

**Signature:** `Cursor::starts_with self:Cursor prefix:str -> bool`

**Stack effect:** `Cursor str -> bool`

```casa
"hello" Cursor::new = cursor
"hel" cursor.starts_with print    # true
"xyz" cursor.starts_with print    # false
```

### `Cursor::expect_char`

Consumes the next character if it matches `expected`. Returns `Result::Ok` with the character on success, or `Result::Error` with a `ParseError` on mismatch or EOF.

**Signature:** `Cursor::expect_char self:Cursor expected:char -> Result[char ParseError]`

**Stack effect:** `Cursor char -> Result[char ParseError]`

```casa
"abc" Cursor::new = cursor
'a' cursor.expect_char .unwrap print    # a
```

### `Cursor::skip`

Advances the cursor position by `n` characters without returning them.

**Signature:** `Cursor::skip self:Cursor n:int`

**Stack effect:** `Cursor int -> None`

```casa
"hello" Cursor::new = cursor
3 cursor.skip
cursor.peek .unwrap print    # l
```

### `Cursor::take_string`

Consumes the exact target string if the remaining input starts with it. Returns `Result::Ok` with the matched string on success, or `Result::Error` with a `ParseError` on mismatch.

**Signature:** `Cursor::take_string self:Cursor target:str -> Result[str ParseError]`

**Stack effect:** `Cursor str -> Result[str ParseError]`

```casa
"hello world" Cursor::new = cursor
"hello" cursor.take_string .unwrap print    # hello
```

### `Cursor::skip_while`

Advances the cursor while the predicate returns `true` for the current character.

**Signature:** `Cursor::skip_while self:Cursor pred:fn[char -> bool]`

**Stack effect:** `Cursor fn[char -> bool] -> None`

```casa
"   hello" Cursor::new = cursor
{ .is_space } cursor.skip_while
cursor.peek .unwrap print    # h
```

### `Cursor::take_while`

Collects characters while the predicate returns `true`, returning them as a substring. Uses `str::substring` internally, so no character-by-character allocation is needed.

**Signature:** `Cursor::take_while self:Cursor pred:fn[char -> bool] -> str`

**Stack effect:** `Cursor fn[char -> bool] -> str`

```casa
"abc123" Cursor::new = cursor
{ .is_alpha } cursor.take_while print    # abc
{ .is_digit } cursor.take_while print    # 123
```

### `Cursor::save`

Returns the current cursor position for later backtracking.

**Signature:** `Cursor::save self:Cursor -> int`

**Stack effect:** `Cursor -> int`

### `Cursor::restore`

Sets the cursor position back to a previously saved value.

**Signature:** `Cursor::restore self:Cursor saved:int`

**Stack effect:** `Cursor int -> None`

```casa
"test" Cursor::new = cursor
cursor.save = saved
cursor.advance drop
cursor.advance drop
saved cursor.restore
cursor.peek .unwrap print    # t
```

## Helper Functions

### `chars_to_str`

Converts a `List[char]` to a `str`. Allocates a new string with the correct length and null terminator.

**Signature:** `chars_to_str chars:List[char] -> str`

**Stack effect:** `List[char] -> str`

### `str_to_int`

Converts a digit string to an integer. Handles optional leading `-` for negative numbers.

**Signature:** `str_to_int s:str -> int`

**Stack effect:** `str -> int`

```casa
"42" str_to_int print      # 42
"-7" str_to_int print      # -7
```

### `is_ident_start`

Returns `true` if the character is alphabetic or an underscore. Suitable for the first character of an identifier.

**Signature:** `is_ident_start c:char -> bool`

**Stack effect:** `char -> bool`

### `is_ident_char`

Returns `true` if the character is alphanumeric or an underscore. Suitable for subsequent characters of an identifier.

**Signature:** `is_ident_char c:char -> bool`

**Stack effect:** `char -> bool`

## High-Level Parsers

### `skip_whitespace`

Skips spaces, tabs, newlines, and carriage returns.

**Signature:** `skip_whitespace cursor:Cursor`

**Stack effect:** `Cursor -> None`

```casa
"   hello" Cursor::new = cursor
cursor skip_whitespace
cursor.pos print    # 3
```

### `parse_int`

Parses an integer with optional leading `-`. Returns `Result::Error` if no digits are found. Restores cursor position on failure.

**Signature:** `parse_int cursor:Cursor -> Result[int ParseError]`

**Stack effect:** `Cursor -> Result[int ParseError]`

```casa
"42" Cursor::new parse_int .unwrap print      # 42
"-7" Cursor::new parse_int .unwrap print      # -7
"abc" Cursor::new parse_int .is_error print   # true
```

### `parse_identifier`

Parses an identifier matching `[a-zA-Z_][a-zA-Z0-9_]*`. Returns `Result::Error` if the current character is not a valid identifier start. Restores cursor position on failure.

**Signature:** `parse_identifier cursor:Cursor -> Result[str ParseError]`

**Stack effect:** `Cursor -> Result[str ParseError]`

```casa
"my_var" Cursor::new parse_identifier .unwrap print    # my_var
```

### `parse_escape`

Parses an escape sequence after the `\` has been consumed. Recognizes: `\n`, `\t`, `\\`, `\"`, `\'`, `\0`, `\r`, `\{`, `\}`.

**Signature:** `parse_escape cursor:Cursor -> Result[char ParseError]`

**Stack effect:** `Cursor -> Result[char ParseError]`

### `parse_quoted_string`

Parses a double-quoted string with escape sequences. Consumes the opening and closing `"` characters. Returns the string contents (with escapes processed).

**Signature:** `parse_quoted_string cursor:Cursor -> Result[str ParseError]`

**Stack effect:** `Cursor -> Result[str ParseError]`

### `parse_char_literal`

Parses a single-quoted character literal with escape sequences. Consumes the opening and closing `'` characters. Returns the character value.

**Signature:** `parse_char_literal cursor:Cursor -> Result[char ParseError]`

**Stack effect:** `Cursor -> Result[char ParseError]`

## Complete Example

```casa
import "path/to/lib/std.casa"
import "path/to/lib/parser.casa"

# Parse integers and identifiers from input
"abc123" Cursor::new = cursor
{ .is_alpha } cursor.take_while = letters
{ .is_digit } cursor.take_while = digits
letters print    # abc
digits print     # 123

# Parse with backtracking
"test" Cursor::new = cursor2
cursor2.save = pos
&is_ident_char cursor2.take_while print    # test
pos cursor2.restore
{ .is_alpha } cursor2.take_while print   # test
```

See [`examples/parser.casa`](../examples/parser.casa) for a full program demonstrating all parser library features.
