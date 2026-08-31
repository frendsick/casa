# Text, Characters, and Output

Import `std` for the methods and functions on this page:

```casa
import "std" { Bytes Iter List Option String }
```

Source files and string literals contain valid UTF-8. `str` is an immutable
view. `String` owns growable text and releases it during destruction. String
indexes and lengths use bytes. Character iteration and reversal decode Unicode
scalar values. Character classification and case conversion cover ASCII.

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

| Method | Result or action |
|---|---|
| `length self:$str -> u64` | Length in bytes |
| `is_empty self:$str -> bool` | Whether the string has no bytes |
| `at self:$str index:u64 -> char` | Byte at `index`, represented as `char` |
| `eq self:$str other:$str -> bool` | Content equality. `==` is the usual form |
| `substring length:u64 start:u64 self:$str -> String` | Copy a byte range on UTF-8 boundaries |
| `find needle:$str self:$str -> i64` | First byte index, or `-1` |
| `starts_with prefix:$str self:$str -> bool` | Whether text starts with a prefix |
| `ends_with suffix:$str self:$str -> bool` | Whether text ends with a suffix |
| `concat suffix:$str self:$str -> String` | Concatenated owned text |
| `contains needle:$str self:$str -> bool` | Whether text contains a substring |
| `split delimiter:$str self:$str -> List[String]` | Copy split parts |
| `trim self:$str -> String` | Copy without surrounding ASCII whitespace |
| `replace old:$str replacement:$str self:$str -> String` | Replace all matches |
| `to_upper self:$str -> String` | Copy with ASCII letters uppercased |
| `to_lower self:$str -> String` | Copy with ASCII letters lowercased |
| `repeat count:u64 self:$str -> String` | Repeat text |
| `reverse self:$str -> String` | Reverse Unicode scalar values |
| `iter self:$str -> Iter[char]` | Iterator over Unicode scalar values |
| `to_str self:$str -> String` | Allocate an independent owner |

Functions with more than one string argument are often clearest with qualified
names:

```casa
"hello" 1 3 str::substring print    # ell
"," "a,b,c".split = parts
```

`List[String]::join_strings` joins owned parts:

```casa
"a,b,c" "," str::split = parts
", " parts.join_strings print    # a, b, c
```

## Owned strings

`String` is non-`Copy` and moves by default. Use `clone` when you need an
independent owner. `as_str` returns a borrowed view without allocation.

| Method | Result or action |
|---|---|
| `String::new -> String` | Empty owned text |
| `String::with_capacity capacity:u64 -> String` | Empty text with reserved byte capacity |
| `String::from_str text:$str -> String` | Copy a view into owned storage |
| `as_str self:$String -> $str` | Borrow the current text without allocation |
| `length self:$String -> u64` | Length in bytes |
| `capacity self:$String -> u64` | Byte capacity |
| `reserve self:mut$String additional:u64` | Reserve space after the current text |
| `append self:mut$String text:$str` | Append a borrowed view |
| `append_string self:mut$String text:String` | Append and consume owned text |
| `push self:mut$String character:char` | Append one Unicode scalar value |
| `clear self:mut$String` | Remove all text and retain capacity |
| `clone self:$String -> String` | Allocate an independent owner |

```casa
"Hello".to_str = message
", " message.append
'世' message.push
'界' message.push
message.as_str print
```

## Parse text

| Method | Result |
|---|---|
| `to_int self:$str -> Option[i64]` | Signed decimal integer |
| `to_f32 self:$str -> Option[f32]` | 32-bit decimal floating-point value |
| `to_f64 self:$str -> Option[f64]` | 64-bit decimal floating-point value |

Malformed input returns `Option::None`. Integer parsing does not ignore
whitespace, so call `trim` first when needed. Floating-point parsing accepts
decimal exponents, signed zero, `inf`, `-inf`, and `NaN`. Finite decimal text
rounds to the nearest value of the target width, with ties rounded to even.

```casa
" -42 ".trim.to_int .unwrap print
"1.5e3".to_f64 .unwrap print
```

## Convert bytes to text

`Bytes.to_str self:$Bytes -> Result[String Utf8Error]` validates UTF-8 and
copies valid bytes into an owned `String`. It borrows the byte buffer, so the
source remains available after conversion. Invalid UTF-8 returns `Utf8Error`.
Casa does not provide a consuming `into_str` conversion.

```casa
Bytes::new = bytes
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

| Method | Result or action |
|---|---|
| `codepoint self:char -> u32` | Unicode scalar value |
| `char::from_codepoint value:u32 -> Option[char]` | Validated character |
| `unsafe fn char::from_codepoint_unchecked value:u32 -> char` | Character without validation |
| `is_digit self:char -> bool` | ASCII digit |
| `is_upper self:char -> bool` | ASCII uppercase letter |
| `is_lower self:char -> bool` | ASCII lowercase letter |
| `is_alpha self:char -> bool` | ASCII letter |
| `is_space self:char -> bool` | ASCII space, tab, newline, or carriage return |
| `eq self:char other:char -> bool` | Equality |
| `lt self:char other:char -> bool` | Codepoint ordering |

```casa
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
| `print` | Standard output |
| `println text:$str` | Standard output, then newline |
| `println_string text:String` | Consume owned text and write it with a newline |
| `eprint text:$str` | Standard error |
| `eprintln text:$str` | Standard error, then newline |
| `eprintln_string text:String` | Consume owned text and write it to standard error with a newline |

```casa
"ready" println
"warning" eprintln
```

`Display.to_str` and string interpolation produce owned `String` values:

```casa
42.to_str = answer
f"answer: {answer}" println_string
```

See the [built-in trait catalog](traits.md#built-in-traits) for displayable
types and [Types and Literals](types-and-literals.md#string-interpolation) for
f-strings.

## C strings and mutable buffers

`as_cstr` checks for interior NUL and returns an optional borrowed
NUL-terminated byte view for system interfaces. `$cstr` does not own or free
its storage. `to_str` validates UTF-8 and copies the bytes into owned Casa text:

```casa
"hello".as_cstr.unwrap = raw:$cstr
raw.to_str.unwrap.as_str print
```

Constructing `$cstr` from a raw pointer requires `unsafe` code to guarantee an
accessible NUL terminator and a live origin. Use `to_bytes` when the bytes do
not have a text guarantee. `$cstr` has no direct comparison, formatting, or
printing operations.
