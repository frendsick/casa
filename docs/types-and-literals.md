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
| `str` | An immutable UTF-8 text view |
| `$cstr` | A borrowed NUL-terminated foreign byte string |

An unconstrained integer literal defaults to `i64`. An unconstrained
floating-point literal defaults to `f64`. A nearby annotation or parameter can
select another width:

```casa
255 = byte:u8
1.5 = ratio:f32
42 typeof print       # i64
```

Floating-point values use partial equality and ordering because NaN is
unordered. Integer values use total equality and ordering.

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

A string literal is a copied `str` view of static read-only storage. Convert it
with `.to_str` when you need an owned, growable `String`:

```casa
"Casa".to_str = name:String
'!' name.push
name.as_str print
```

```casa
'\u{1F600}' print       # 😀
"\u{3BB}" print         # λ
```

`cstr` is only used through a shared `$cstr` borrow. It has no literal syntax,
does not own its storage, and does not assume UTF-8. Convert a `str` when an
operating-system or C interface needs a NUL-terminated byte string:

```casa
"hello".as_cstr.unwrap = message:$cstr
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
| `array[T N]` | An owned sequence of exactly `N` elements |
| `Slice[T]` | A borrowed sequence with a runtime length |
| `List[T]` | An owned growable sequence |
| `Bytes` | An owned growable byte buffer |
| `String` | Owned growable UTF-8 text |
| `fn[inputs -> outputs]` | A function value |
| `Option[T]` | A value that can be absent |
| `Result[T E]` | A success value or an error |
| Struct name | A user-defined product type |
| Enum name | A user-defined variant type |

Array literals infer one common element type, and their length is part of the
type:

```casa
[1, 2, 3] = numbers:array[i64 3]
[] = names:array[str 0]
```

An `array[T N]` is `Copy` when `T` is `Copy`, including when `N` is zero.
Arrays with non-`Copy` elements move by default.

The annotation gives an empty array its element type. Use `Bytes` for compact
binary data. It stores one `u8` per byte and has no byte-literal syntax. Every
evaluation of an array literal produces an independent owned array that takes
ownership of its elements. `Slice[T]` borrows a runtime-length range from a `List[T]`. See
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

Casa has no general `(Type)` cast. Use a typed binding when a literal or generic
construction needs context. Use the named raw pointer and typed-memory
operations for representation boundaries.

## Comments

`#` starts a comment that continues to the end of the line:

```casa
# Print the answer.
42 print
```

Casa has no block comments.
