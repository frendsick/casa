# Text, Characters, and Output

Import `std` for the methods and functions on this page:

```casa
import "std"
```

Strings use byte indexes. Character classification and case conversion cover
ASCII.

## String API

| Method | Result or action |
|---|---|
| `length self:str -> u64` | Length in bytes |
| `at self:str index:u64 -> char` | Character at a byte index |
| `eq self:str other:str -> bool` | Content equality. `==` is the usual form |
| `substring length:u64 start:u64 self:str -> str` | Copy a byte range |
| `find needle:str self:str -> i64` | First byte index, or `-1` |
| `starts_with prefix:str self:str -> bool` | Whether text starts with a prefix |
| `ends_with suffix:str self:str -> bool` | Whether text ends with a suffix |
| `concat suffix:str self:str -> str` | Concatenated text |
| `contains needle:str self:str -> bool` | Whether text contains a substring |
| `split delimiter:str self:str -> List[str]` | Split text into parts |
| `trim self:str -> str` | Remove surrounding ASCII whitespace |
| `replace old:str replacement:str self:str -> str` | Replace all matches |
| `to_upper self:str -> str` | Uppercase ASCII letters |
| `to_lower self:str -> str` | Lowercase ASCII letters |
| `repeat count:u64 self:str -> str` | Repeat text |
| `reverse self:str -> str` | Reverse the bytes |
| `iter self:str -> Iter[char]` | Iterator over characters |

Functions with more than one string argument are often clearest with qualified
names:

```casa
"hello" 1 3 str::substring print    # ell
"," "a,b,c".split = parts
```

`List[str]::join` performs the inverse of `split`:

```casa
["a", "b", "c"] List::from_array = parts
", " parts.join print    # a, b, c
```

## Parse text

| Method | Result |
|---|---|
| `to_int self:str -> Option[i64]` | Signed decimal integer |
| `to_f32 self:str -> Option[f32]` | 32-bit decimal floating-point value |
| `to_f64 self:str -> Option[f64]` | 64-bit decimal floating-point value |

Malformed input returns `Option::None`. Integer parsing does not ignore
whitespace, so call `trim` first when needed. Floating-point parsing accepts
decimal exponents, signed zero, `inf`, `-inf`, and `NaN`.

```casa
" -42 ".trim.to_int .unwrap print
"1.5e3".to_f64 .unwrap print
```

## Characters

| Method | Result or action |
|---|---|
| `codepoint self:char -> u32` | Unicode scalar value |
| `char::from_codepoint value:u32 -> Option[char]` | Validated character |
| `char::from_codepoint_unchecked value:u32 -> char` | Character without validation |
| `is_digit self:char -> bool` | ASCII digit |
| `is_upper self:char -> bool` | ASCII uppercase letter |
| `is_lower self:char -> bool` | ASCII lowercase letter |
| `is_alpha self:char -> bool` | ASCII letter |
| `is_space self:char -> bool` | ASCII space, tab, newline, or carriage return |
| `eq self:char other:char -> bool` | Equality |
| `lt self:char other:char -> bool` | Codepoint ordering |

```casa
'A'.codepoint print          # 65
'7'.is_digit print           # true
65 = value:u32
value char::from_codepoint .unwrap print
```

## Formatting and output

`print` writes any value that implements `Display`. `println`, `eprint`, and
`eprintln` accept strings:

| Function | Destination |
|---|---|
| `print` | Standard output |
| `println text:str` | Standard output, then newline |
| `eprint text:str` | Standard error |
| `eprintln text:str` | Standard error, then newline |

```casa
"ready" println
"warning" eprintln
```

Use `.to_str` on a `Display` value, or use string interpolation:

```casa
42.to_str = answer
f"answer: {answer}" println
```

See the [built-in trait catalog](traits.md#built-in-traits) for displayable
types and [Types and Literals](types-and-literals.md#string-interpolation) for
f-strings.

## C strings and mutable buffers

`as_cstr` returns a null-terminated view for system interfaces. `to_str`
copies a `cstr` into Casa text:

```casa
"hello".as_cstr = raw:cstr
raw.to_str print
```

Low-level code can use `str::from_cstr pointer:ptr -> str` and
`set self:str index:u64 character:char` when it owns a suitable buffer. These
operations do not validate buffer capacity. Prefer normal string operations in
application code.
