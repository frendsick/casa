# Types and Literals

Casa is a statically typed language. Types are checked at compile time and most can be inferred automatically — you rarely need to write type annotations outside of function signatures.

## Primitive Types

### `int`

Integer type. All integers are 64-bit signed values.

```casa
42 print       # 42
-42 print      # -42
```

Convert to string with `.to_str` (see [Standard Library](standard-library.md#type-conversions)).

### `bool`

Boolean type with two values: `true` and `false`.

```casa
true print     # 1
false print    # 0
```

Booleans are printed as integers (`1` for true, `0` for false). Convert to `"true"` or `"false"` with `.to_str` (see [Standard Library](standard-library.md#type-conversions)).

### `char`

Single character type. Character literals are enclosed in single quotes.

```casa
'A' print      # A
'z' print      # z
```

Characters support the following escape sequences:

| Sequence | Meaning |
|----------|---------|
| `\n` | Newline |
| `\t` | Tab |
| `\\` | Literal backslash |
| `\'` | Single quote |
| `\0` | Null byte |
| `\r` | Carriage return |

```casa
'\n' print     # prints a newline
'\t' print     # prints a tab
'\\' print     # prints a backslash
'\'' print     # prints a single quote
```

Invalid escape sequences (e.g. `\q`) produce a compile-time `SYNTAX` error. Empty char literals (`''`) are also errors.

Characters can be cast to `int` to get their ASCII code:

```casa
'A' (int) print    # 65
```

### `str`

String type. String literals are enclosed in double quotes. Internally, strings are stored as `[8-byte length][N bytes data][\0]`, with the pointer pointing to the length prefix.

```casa
"Hello world!" print
```

Convert to string with `.to_str` (identity, see [Standard Library](standard-library.md#type-conversions)).

Strings support the following escape sequences:

| Sequence | Meaning |
|----------|---------|
| `\n` | Newline |
| `\t` | Tab |
| `\\` | Literal backslash |
| `\"` | Double quote |
| `\0` | Null byte |
| `\r` | Carriage return |
| `\{` | Literal left brace |
| `\}` | Literal right brace |

```casa
"hello\nworld" print    # prints on two lines
"say \"hi\"" print      # say "hi"
"col1\tcol2" print      # tab-separated
```

Invalid escape sequences (e.g. `\q`) produce a compile-time `SYNTAX` error.

### `cstr`

Null-terminated C string type. There is no literal syntax for `cstr`. Create one from a `str` using `str::as_cstr` (see [Standard Library](standard-library.md#stras_cstr)). The `cstr` points directly to the byte data (no length prefix), terminated by a null byte.

```casa
"hello" .as_cstr print    # hello
```

Convert to `str` with `cstr::to_str` (see [Standard Library](standard-library.md#cstrto_str)).

### F-Strings (String Interpolation)

F-strings let you embed expressions inside a string literal. Prefix a string with `f` and wrap expressions in `{}`.

**Stack effect:** `-> str`

```casa
"world" = name
f"hello {name}" print    # hello world
```

Expressions inside `{}` must produce a value of type `str`. Non-string types are not auto-converted.

```casa
# This will NOT work: int expression inside f-string with text
42 = n
f"count: {n}"    # TYPE error: expected str, got int
```

Multiple expressions are supported:

```casa
"Alice" = first
"Smith" = last
f"{first} {last}" print    # Alice Smith
```

Use `\{` and `\}` to produce literal braces:

```casa
f"\{x\}" print    # {x}
```

Escape sequences work inside f-strings just like regular strings (`\n`, `\t`, `\\`, `\"`, `\0`, `\r`):

```casa
f"line1\nline2" print
# line1
# line2
```

An f-string with no expressions is equivalent to a regular string:

```casa
f"hello"    # same as "hello"
```

## Composite Types

### `ptr`

Heap pointer returned by `alloc`. Used with sized load/store intrinsics (`load8`/`load16`/`load32`/`load64` and `store8`/`store16`/`store32`/`store64`) for byte-addressed memory access. Load/store intrinsics use absolute addressing.

```casa
32 alloc = buffer              # allocate 32 bytes
42 buffer (ptr) store64        # store 64-bit value at buffer
buffer (ptr) load64 print      # 42
```

Convert to string with `.to_str` (see [Standard Library](standard-library.md#type-conversions)).

Pointer arithmetic is supported with `+` and `-` using byte offsets:

```casa
32 alloc = buffer
99 buffer (ptr) 8 + store64    # store 99 at byte offset 8
buffer (ptr) 8 + load64 print  # 99
```

See [Functions and Lambdas — Memory Intrinsics](functions-and-lambdas.md#memory-intrinsics) for details.

### `array[T]`

Fixed-size, statically typed array literal. The element type `T` is inferred from the items in the array.

```casa
[1, 2, 3]          # type: array[int]
["a", "b", "c"]    # type: array[str]
[true, false]       # type: array[bool]
```

Array items can be literals or variables:

```casa
42 = x
[x, 2, 3]          # type: array[int]
```

All items must have the same type. Heterogeneous arrays are compile-time errors:

```casa
[1, "hello"]        # TYPE_MISMATCH error
```

An empty array has type `array[any]`:

```casa
[]                  # type: array[any]
```

Arrays can be nested. The element type is inferred recursively:

```casa
[[1, 2], [3, 4]]   # type: array[array[int]]
```

The bare type name `array` matches any `array[T]` for backward compatibility (e.g. in function signatures).

The array's length is stored in the first 8 bytes (64-bit value). Elements are stored starting at byte offset 8, each taking 8 bytes.

See [Standard Library — Arrays](standard-library.md#arrays) for `array::length` and `array::nth`.

### `fn[sig]`

Function type representing a lambda or function reference. The signature inside the brackets describes the parameter and return types.

```casa
{ 2 * }           # type: fn[int -> int]
{ 1 + }           # type: fn[int -> int]
{ drop "hi" }     # type: fn[any -> str]
```

Call a function value with `exec`:

```casa
{ 2 * } = double
21 double exec print   # 42
```

`fn[sig]` can be used as a parameter type in function declarations, allowing functions to accept callbacks:

```casa
fn apply f:fn[int -> int] x:int -> int {
    x f exec
}

40 { 2 + } apply print   # 42
```

See [Functions and Lambdas](functions-and-lambdas.md#lambdas) for details.

### `option[T]`

Optional type representing a value that may or may not be present. Built with the `some` and `none` constructors.

`none` pushes an empty option with bare type `option`. It is compatible with any `option[T]`.

**Stack effect:** `-> option`

`some` wraps the top-of-stack value into an option. The resulting type is `option[T]` where `T` is the type of the wrapped value.

**Stack effect:** `T -> option[T]`

```casa
42 some          # type: option[int]
"hello" some     # type: option[str]
none             # type: option (compatible with any option[T])
```

At runtime, an option is heap-allocated as 16 bytes: `[tag, value]` where each field is 8 bytes. The tag is `1` for `Some` and `0` for `None`.

Options stored in variables retain their type:

```casa
42 some = x      # x has type option[int]
none = y         # y has type option (bare)
```

A bare `option` type matches any `option[T]` in function signatures, similar to how bare `array` matches any `array[T]`:

```casa
fn check opt:option -> bool { true }
42 some check    # works: option[int] matches bare option
```

`none` and `some` can appear in different branches of a conditional. The type checker unifies them to the more specific `option[T]`:

```casa
fn safe_head arr:array[int] -> option[int] {
    if 0 arr .length > then
        0 arr array::nth some
    else
        none
    fi
}
```

See [Standard Library -- Option](standard-library.md#option) for `is_some`, `is_none`, `unwrap`, and `unwrap_or`.

### User-Defined Structs

Struct names are types. After defining a struct, its name can be used as a type.

```casa
struct Point {
    x: int
    y: int
}

10 20 Point = p    # p has type Point
```

See [Structs and Methods](structs-and-methods.md) for details.

## Type Variables (Generics)

Type variables let functions declare type relationships between inputs and outputs. They are declared in square brackets after the function name and are resolved to concrete types at each call site.

```casa
fn id[T] T -> T { }
42 id        # T=int, returns int
"hi" id      # T=str, returns str
```

Type variables are purely compile-time — they have no runtime cost. See [Functions and Lambdas — Generic Functions](functions-and-lambdas.md#generic-functions) for details.

## The `any` Type

`any` is a special wildcard type that matches any other type. It is used as an escape hatch when the type system cannot determine a precise type.

```casa
32 alloc = buffer
42 buffer (ptr) store64
buffer (ptr) load64    # type: int
```

## Type Casting

The `(TypeName)` syntax casts the top of the stack to the given type. This is a compile-time annotation only — no runtime check is performed.

**Stack effect:** `a -> TypeName`

```casa
buffer (ptr) load64 (int)    # cast int -> int (no-op here, but useful for generic data)
```

This is useful when working with generic data structures that return `any`.

## Comments

Line comments start with `#` and extend to the end of the line.

```casa
# This is a comment
42 print  # This is also a comment
```

There are no block comments.
