# Text, Characters, and Output

Import `std` for the methods and functions on this page:

```casa
import "std"
```

Source files and string literals contain valid UTF-8. `str` is an immutable
view. `String` owns growable text and releases it during destruction. String
indexes and lengths use bytes. Character iteration and reversal decode Unicode
scalar values. Character classification and case conversion cover ASCII.

Reference tables abbreviate library type names and list inputs in consumption
order. Source examples use qualified names. See [reference notation](notation.md)
for signatures, fragments, and commands for running complete examples.

## Choose a text type

- Use `str` for literals, read-only parameters, and views into existing text.
- Prefer a `$str` parameter when a function only reads text. A caller can pass a
  literal or use `String.as_str` without allocation.
- Use `String` when text must grow, be retained independently of an input
  borrow, move into an owning value, or be returned as newly constructed text.
- Return `str` only for static storage or a view tied to an input lifetime.
  Return `String` for allocated or assembled text.
- Convert explicitly at ownership boundaries. Do not allocate a `String` only
  to pass read-only text to a function.

This example passes both static and owned text to one read-only operation:

```casa
import "std"

fn print_length text:$str { text.length print }

"literal" print_length
"owned".to_str = text
text.as_str print_length
```

## Text views

| Method | Signature | Behavior |
|---|---|---|
| [at](#strat) | `fn at s:$str index:u64 -> char` | Byte at `index`, represented as `char` |
| [concat](#strconcat) | `fn concat b:$str a:$str -> String` | Concatenated owned text |
| [contains](#strcontains) | `fn contains needle:$str s:$str -> bool` | Whether text contains a substring |
| [ends_with](#strends_with) | `fn ends_with suffix:$str s:$str -> bool` | Whether text ends with a suffix |
| [eq](#streq) | `fn eq b:$str a:$str -> bool` | Content equality. `==` is the usual form |
| [find](#strfind) | `fn find needle:$str s:$str -> i64` | First byte index, or `-1` |
| [is_empty](#stris_empty) | `fn is_empty self:$str -> bool` | Whether the string has no bytes |
| [iter](#striter) | `fn iter self:$str -> Iter[char]` | Iterator over Unicode scalar values |
| [length](#strlength) | `fn length s:$str -> u64` | Length in bytes |
| [repeat](#strrepeat) | `fn repeat self:$str n:u64 -> String` | Repeat text |
| [replace](#strreplace) | `fn replace old:$str new_str:$str s:$str -> String` | Replace all matches |
| [reverse](#strreverse) | `fn reverse self:$str -> String` | Reverse Unicode scalar values |
| [split](#strsplit) | `fn split delimiter:$str s:$str -> List[String]` | Copy split parts |
| [starts_with](#strstarts_with) | `fn starts_with prefix:$str s:$str -> bool` | Whether text starts with a prefix |
| [substring](#strsubstring) | `fn substring len:u64 start:u64 s:$str -> String` | Copy a byte range on UTF-8 boundaries |
| [to_lower](#strto_lower) | `fn to_lower self:$str -> String` | Copy with ASCII letters lowercased |
| [to_str](#strto_str) | `fn to_str self:$str -> String` | Allocate an independent owner |
| [to_upper](#strto_upper) | `fn to_upper self:$str -> String` | Copy with ASCII letters uppercased |
| [trim](#strtrim) | `fn trim s:$str -> String` | Copy without surrounding ASCII whitespace |

<a id="strat"></a>

### at

```text
fn at s:$str index:u64 -> char
```

Returns the byte at `index`, represented as `char`.

<a id="strconcat"></a>

### concat

```text
fn concat b:$str a:$str -> String
```

Returns owned text containing both inputs.

<a id="strcontains"></a>

### contains

```text
fn contains needle:$str s:$str -> bool
```

Returns whether text contains a substring.

<a id="strends_with"></a>

### ends_with

```text
fn ends_with suffix:$str s:$str -> bool
```

Returns whether text ends with a suffix.

<a id="streq"></a>

### eq

```text
fn eq b:$str a:$str -> bool
```

Compares text content for equality. `==` is the usual form.

<a id="strfind"></a>

### find

```text
fn find needle:$str s:$str -> i64
```

Returns the first matching byte index, or `-1` when no match exists.

<a id="stris_empty"></a>

### is_empty

```text
fn is_empty self:$str -> bool
```

Returns whether the string has no bytes.

<a id="striter"></a>

### iter

```text
fn iter self:$str -> Iter[char]
```

Returns an iterator over Unicode scalar values.

<a id="strlength"></a>

### length

```text
fn length s:$str -> u64
```

Returns the length in bytes.

<a id="strrepeat"></a>

### repeat

```text
fn repeat self:$str n:u64 -> String
```

Returns owned text containing `n` repetitions of the input.

Call: `2 "abc".repeat`, producing `abcabc`.

<a id="strreplace"></a>

### replace

```text
fn replace old:$str new_str:$str s:$str -> String
```

Replaces all matches.

<a id="strreverse"></a>

### reverse

```text
fn reverse self:$str -> String
```

Reverses Unicode scalar values.

<a id="strsplit"></a>

### split

```text
fn split delimiter:$str s:$str -> List[String]
```

Copies the parts separated by `delimiter` into a list of owned strings.

Call: `"a,b,c" "," str::split`.

<a id="strstarts_with"></a>

### starts_with

```text
fn starts_with prefix:$str s:$str -> bool
```

Returns whether text starts with a prefix.

<a id="strsubstring"></a>

### substring

```text
fn substring len:u64 start:u64 s:$str -> String
```

Copies `len` bytes starting at byte offset `start` into owned text. Both range
boundaries must be UTF-8 boundaries, and the range must fit within the input. An invalid
range terminates the program.

Call: `"hello" 1 3 str::substring`, producing `ell`.

<a id="strto_lower"></a>

### to_lower

```text
fn to_lower self:$str -> String
```

Copies with ASCII letters lowercased.

<a id="strto_str"></a>

### to_str

```text
fn to_str self:$str -> String
```

Allocates an independent owner.

<a id="strto_upper"></a>

### to_upper

```text
fn to_upper self:$str -> String
```

Copies with ASCII letters uppercased.

<a id="strtrim"></a>

### trim

```text
fn trim s:$str -> String
```

Copies without surrounding ASCII whitespace.

### Text call examples

Functions with more than one string argument are often clearest with qualified
names:

```casa
import "std"

"hello" 1 3 str::substring print    # ell
"a,b,c" "," str::split = parts
```

`List[String]::join_strings` joins owned parts:

```casa
import "std"

"a,b,c" "," str::split = parts
", " parts.join_strings print    # a, b, c
```

## Owned strings

`String` is non-`Copy` and moves by default. Use `clone` when you need an
independent owner. `as_str` returns a borrowed view without allocation.

| Method | Signature | Behavior |
|---|---|---|
| [append](#stringappend) | `fn append self:mut$String text:$str` | Append a borrowed view |
| [append_string](#stringappend_string) | `fn append_string self:mut$String text:String` | Append and consume owned text |
| [as_str](#stringas_str) | `fn as_str self:$String -> $str` | Borrow the current text without allocation |
| [capacity](#stringcapacity) | `fn capacity self:$String -> u64` | Byte capacity |
| [clear](#stringclear) | `fn clear self:mut$String` | Remove all text and retain capacity |
| [clone](#stringclone) | `fn clone self:$String -> String` | Allocate an independent owner |
| [from_str](#stringfrom_str) | `fn from_str text:$str -> String` | Copy a view into owned storage |
| [length](#stringlength) | `fn length self:$String -> u64` | Length in bytes |
| [new](#stringnew) | `fn new -> String` | Empty owned text |
| [push](#stringpush) | `fn push self:mut$String character:char` | Append one Unicode scalar value |
| [reserve](#stringreserve) | `fn reserve self:mut$String additional:u64` | Reserve space after the current text |
| [with_capacity](#stringwith_capacity) | `fn with_capacity capacity:u64 -> String` | Empty text with reserved byte capacity |

<a id="stringappend"></a>

### append

```text
fn append self:mut$String text:$str
```

Appends a borrowed view.

<a id="stringappend_string"></a>

### append_string

```text
fn append_string self:mut$String text:String
```

Appends and consumes owned text.

<a id="stringas_str"></a>

### as_str

```text
fn as_str self:$String -> $str
```

Borrows the current text without allocation.

<a id="stringcapacity"></a>

### capacity

```text
fn capacity self:$String -> u64
```

Returns the byte capacity.

<a id="stringclear"></a>

### clear

```text
fn clear self:mut$String
```

Removes all text and retains capacity.

<a id="stringclone"></a>

### clone

```text
fn clone self:$String -> String
```

Allocates an independent owner.

<a id="stringfrom_str"></a>

### from_str

```text
fn from_str text:$str -> String
```

Copies a view into owned storage.

<a id="stringlength"></a>

### length

```text
fn length self:$String -> u64
```

Returns the length in bytes.

<a id="stringnew"></a>

### new

```text
fn new -> String
```

Creates an empty owned text.

<a id="stringpush"></a>

### push

```text
fn push self:mut$String character:char
```

Appends one Unicode scalar value.

<a id="stringreserve"></a>

### reserve

```text
fn reserve self:mut$String additional:u64
```

Reserves space after the current text.

<a id="stringwith_capacity"></a>

### with_capacity

```text
fn with_capacity capacity:u64 -> String
```

Creates an empty text with reserved byte capacity.

### String example

```casa
import "std"

"Hello".to_str = message
", " message.append
'世' message.push
'界' message.push
message.as_str print
```

## Parse text

| Method | Signature | Behavior |
|---|---|---|
| [to_f32](#strto_f32) | `fn to_f32 self:$str -> Option[f32]` | 32-bit decimal floating-point value |
| [to_f64](#strto_f64) | `fn to_f64 self:$str -> Option[f64]` | 64-bit decimal floating-point value |
| [to_int](#strto_int) | `fn to_int self:$str -> Option[i64]` | Signed decimal integer |

<a id="strto_f32"></a>

### to_f32

```text
fn to_f32 self:$str -> Option[f32]
```

Parses a 32-bit decimal floating-point value.

<a id="strto_f64"></a>

### to_f64

```text
fn to_f64 self:$str -> Option[f64]
```

Parses a 64-bit decimal floating-point value.

<a id="strto_int"></a>

### to_int

```text
fn to_int self:$str -> Option[i64]
```

Parses a signed decimal integer.

### Parsing example

Malformed input returns `Option::None`. Integer parsing does not ignore
whitespace, so call `trim` first when needed. Floating-point parsing accepts
decimal exponents, signed zero, `inf`, `-inf`, and `NaN`. Finite decimal text
rounds to the nearest value of the target width, with ties rounded to even.

```casa
import "std"

" -42 ".trim = text
text.as_str.to_int = number
number.unwrap print
"1.5e3".to_f64 .unwrap print
```

## Convert text and bytes

`Bytes.to_str self:$Bytes -> Result[String Utf8Error]` validates UTF-8 and
copies valid bytes into an owned `String`. It borrows the byte buffer, so the
source remains available after conversion. Invalid UTF-8 returns `Utf8Error`.
`Bytes::from_str source:$str -> Bytes` copies the text's UTF-8 bytes.
Casa does not provide a consuming `into_str` conversion or an implicit
conversion in either direction.

```casa
import "std"

std::Bytes::new = bytes
72 bytes.push
105 bytes.push
bytes.to_str.unwrap = text
text.as_str print
```

Raw external input stays as bytes until a caller validates it as text. This
includes `file::read_all`, standard-input readers, process arguments,
environment values, and directory entry names. The conversion is explicit so
invalid UTF-8 remains available to binary consumers without replacement or
data loss.

## Characters

| Method | Signature | Behavior |
|---|---|---|
| [codepoint](#charcodepoint) | `fn codepoint self:char -> u32` | Unicode scalar value |
| [eq](#chareq) | `fn eq self:$char other:$char -> bool` | Equality |
| [from_codepoint](#charfrom_codepoint) | `fn from_codepoint codepoint:u32 -> Option[char]` | Validated character |
| [from_codepoint_unchecked](#charfrom_codepoint_unchecked) | `unsafe fn from_codepoint_unchecked value:u32 -> char` | Character without validation |
| [is_alpha](#charis_alpha) | `fn is_alpha c:char -> bool` | ASCII letter |
| [is_digit](#charis_digit) | `fn is_digit c:char -> bool` | ASCII digit |
| [is_lower](#charis_lower) | `fn is_lower c:char -> bool` | ASCII lowercase letter |
| [is_space](#charis_space) | `fn is_space c:char -> bool` | ASCII space, tab, newline, or carriage return |
| [is_upper](#charis_upper) | `fn is_upper c:char -> bool` | ASCII uppercase letter |
| [lt](#charlt) | `fn lt self:$char other:$char -> bool` | Codepoint ordering |

<a id="charcodepoint"></a>

### codepoint

```text
fn codepoint self:char -> u32
```

Returns the Unicode scalar value as `u32`.

<a id="chareq"></a>

### eq

```text
fn eq self:$char other:$char -> bool
```

Compares characters for equality.

<a id="charfrom_codepoint"></a>

### from_codepoint

```text
fn from_codepoint codepoint:u32 -> Option[char]
```

Returns a character if the codepoint is a valid Unicode scalar value. Otherwise, returns
`Option::None`.

<a id="charfrom_codepoint_unchecked"></a>

### from_codepoint_unchecked

```text
unsafe fn from_codepoint_unchecked value:u32 -> char
```

Creates a character without validating the codepoint. The caller must supply a valid
Unicode scalar value.

<a id="charis_alpha"></a>

### is_alpha

```text
fn is_alpha c:char -> bool
```

Returns whether the character is an ASCII letter.

<a id="charis_digit"></a>

### is_digit

```text
fn is_digit c:char -> bool
```

Returns whether the character is an ASCII digit.

<a id="charis_lower"></a>

### is_lower

```text
fn is_lower c:char -> bool
```

Returns whether the character is an ASCII lowercase letter.

<a id="charis_space"></a>

### is_space

```text
fn is_space c:char -> bool
```

Returns whether the character is an ASCII space, tab, newline, or carriage return.

<a id="charis_upper"></a>

### is_upper

```text
fn is_upper c:char -> bool
```

Returns whether the character is an ASCII uppercase letter.

<a id="charlt"></a>

### lt

```text
fn lt self:$char other:$char -> bool
```

Compares characters by codepoint.

### Character conversion

```casa
import "std"

'A'.codepoint print          # 65
'😀'.codepoint print         # 128512
'7'.is_digit print           # true
65 = value:u32
value char::from_codepoint .unwrap print
```

`char::from_codepoint` rejects surrogate values and values above `U+10FFFF`.
The unchecked form requires `unsafe` and has undefined behavior for a value
that is not a Unicode scalar.

## Formatting and output

`print` writes any value that implements `Display`. `println`, `eprint`, and
`eprintln` accept strings:

| Function | Destination |
|---|---|
| `eprint text:$str` | Standard error |
| `eprintln text:$str` | Standard error, then newline |
| `eprintln_string text:String` | Consume owned text and write it to standard error with a newline |
| `print` | Standard output |
| `println text:$str` | Standard output, then newline |
| `println_string text:String` | Consume owned text and write it with a newline |

```casa
import "std"

"ready" std::println
"warning" std::eprintln
```

`Display.to_str` and string interpolation produce owned `String` values:

```casa
import "std"

42.to_str = answer
f"answer: {answer}" std::println_string
```

See the [built-in trait catalog](traits.md#built-in-traits) for displayable
types and [Types and Literals](types-and-literals.md#string-interpolation) for
f-strings.

## C strings and mutable buffers

`$cstr` is a borrowed NUL-terminated byte view for system interfaces. It does
not own or free its storage. [Bytes::as_cstr](collections.md#bytesas_cstr)
provides this view for a byte buffer.

| Method | Signature | Behavior |
|---|---|---|
| [as_cstr](#stras_cstr) | `fn as_cstr s:$str -> Option[$cstr]` | Borrow a NUL-terminated view if no byte is NUL |
| [to_bytes](#cstrto_bytes) | `fn to_bytes self:$cstr -> Bytes` | Copy bytes before the NUL terminator |
| [to_str](#cstrto_str) | `fn to_str self:$cstr -> Result[String Utf8Error]` | Validate and copy UTF-8 text |

<a id="stras_cstr"></a>

### as_cstr

```text
fn as_cstr s:$str -> Option[$cstr]
```

Checks for interior NUL and returns an optional borrowed NUL-terminated byte
view. The view keeps its source loaned until its last use.

<a id="cstrto_bytes"></a>

### to_bytes

```text
fn to_bytes self:$cstr -> Bytes
```

Copies the bytes before the NUL terminator into an independent byte buffer.
Use this method when the bytes do not have a text guarantee.

<a id="cstrto_str"></a>

### to_str

```text
fn to_str self:$cstr -> Result[String Utf8Error]
```

Validates UTF-8 and copies the bytes into owned Casa text. Invalid UTF-8 returns
`Utf8Error`.

### Conversion example

```casa
import "std"

"hello".as_cstr.unwrap = raw:$cstr
raw.to_str.unwrap.as_str print
"path" std::Bytes::from_str = byte_path
byte_path.as_cstr.unwrap = byte_raw:$cstr
```

Constructing `$cstr` from a raw pointer requires `unsafe` code to guarantee an
accessible NUL terminator and a live origin. `$cstr` has no direct comparison,
formatting, or printing operations.
