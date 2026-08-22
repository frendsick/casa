# Types and Literals

Casa checks types at compile time. Literals usually get their type from the
operation or binding that uses them.

## Primitive types

| Type | Values |
|---|---|
| `i8`, `i16`, `i32`, `i64` | Signed integers |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers |
| `f32`, `f64` | Floating-point numbers |
| `bool` | `true` or `false` |
| `char` | One Unicode scalar value |
| `str` | A Casa string |
| `cstr` | A null-terminated C string |

An unconstrained integer literal defaults to `i64`. An unconstrained
floating-point literal defaults to `f64`. A nearby annotation or parameter can
select another width:

```casa
255 = byte:u8
1.5 = ratio:f32
42 typeof print       # i64
```

Numeric operations require matching widths. Casa does not implicitly widen or
narrow stored numeric values.

## Characters and strings

Character literals use single quotes. String literals use double quotes:

```casa
'A' print
'😀' print
"Hallå, 世界" print
```

Both forms support these escapes:

| Escape | Value |
|---|---|
| `\n` | Newline |
| `\t` | Tab |
| `\r` | Carriage return |
| `\0` | Null byte |
| `\\` | Backslash |
| `\xHH` | One ASCII scalar from `00` through `7F` |
| `\u{H...}` | One Unicode scalar written with 1 through 6 hexadecimal digits |

Use `\'` for a quote in a character and `\"` for a quote in a string.
Source files and text literals must be valid UTF-8. A character literal must
contain exactly one Unicode scalar. Surrogates, values above `U+10FFFF`, and
non-ASCII `\x` escapes are compile-time errors.

```casa
'\u{1F600}' print       # 😀
"\u{3BB}" print         # λ
```

`cstr` has no literal syntax. Convert a `str` when an operating-system or C
interface needs a null-terminated string:

```casa
"hello" .as_cstr = message:cstr
```

See [Text and I/O](strings-and-io.md) for string operations and conversions.

## String interpolation

Prefix a string with `f` and put expressions inside braces:

```casa
"Ada" = name
3 = count
f"{name} has {count} tasks\n" print
```

Each expression must implement [`Display`](traits.md#built-in-traits).
Use `\{` and `\}` for literal braces.

## Other types

| Type | Purpose |
|---|---|
| `ptr` | A raw memory address |
| `array[T]` | An owned fixed-length sequence |
| `fn[inputs -> outputs]` | A function value |
| `Option[T]` | A value that can be absent |
| `Result[T E]` | A success value or an error |
| Struct name | A user-defined product type |
| Enum name | A user-defined variant type |

Array literals infer one common element type:

```casa
[1, 2, 3] = numbers:array[i64]
[] = names:array[str]
```

The annotation gives an empty array its element type. Every evaluation of an
array literal produces an independent owned array that takes ownership of its
elements. See
[Collections](collections.md),
[Optional Values and Errors](optional-values-and-errors.md),
[Functions and Lambdas](functions-and-lambdas.md), [Structs and
Methods](structs-and-methods.md), and [Enums](enums.md) for operations on these
types. Raw pointers are covered in [Built-in
Intrinsics](intrinsics.md#advanced-memory-access).

## Constants

Declare a global compile-time value with `const`:

```casa
const LIMIT 100
const GREETING "hello"
```

A constant can use an earlier constant. A block can evaluate supported
operators and `const fn` calls at compile time:

```casa
const fn double value:i64 -> i64 { value 2 * }
const DOUBLE_LIMIT { LIMIT double }
```

A `const fn` can also run like a normal function. Its body cannot use control
flow or global variables, and it can call only other `const fn` functions.

## Numeric conversions

Import `std` to use named numeric conversions:

| Operation | Behavior |
|---|---|
| `Target::from` | Convert every source value exactly |
| `Target::try_from` | Return `Option[Target]` if this value is exact |
| `Target::round_from` | Round to a floating-point target |
| `Target::trunc_from` | Truncate a float to an integer, or terminate if invalid |
| `Target::wrapping_from` | Keep the low integer bits |

```casa
import "std"

120 = byte:i8
byte i16::from = widened

1000 = count:i64
count u8::try_from = maybe_byte
```

Use these operations for numeric conversion. Convert characters with
`.codepoint`, `char::from_codepoint`, or the narrow unsafe
`char::from_codepoint_unchecked` primitive. Representation casts to or from
`char` are compile-time errors.

`(Type)` only changes the
compiler's interpretation of the top stack value. It performs no runtime
conversion or check. This makes casts useful for low-level memory and system
interfaces, but unsafe for general conversion.

## Comments

`#` starts a comment that continues to the end of the line:

```casa
# Print the answer.
42 print
```

Casa has no block comments.
