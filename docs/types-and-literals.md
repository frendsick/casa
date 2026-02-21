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

### `str`

String type. String literals are enclosed in double quotes.

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

### F-Strings (String Interpolation)

F-strings let you embed expressions inside a string literal. Prefix a string with `f` and wrap expressions in `{}`.

**Stack effect:** `( -- str )`

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

Heap pointer returned by `alloc`. Used with `load` and `store` for heap memory access.

```casa
10 alloc = buffer    # allocate 10 heap slots
42 buffer store      # store 42 at buffer
buffer load print    # 42
```

Convert to string with `.to_str` (see [Standard Library](standard-library.md#type-conversions)).

Pointer arithmetic is supported with `+` and `-`:

```casa
10 alloc = buffer
99 buffer 3 + store   # store 99 at offset 3
buffer 3 + load print  # 99
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

The array's length is stored in the first heap slot. Elements are stored starting at offset 1.

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

`any` is a special wildcard type that matches any other type. It is used as an escape hatch when the type system cannot determine a precise type — for example, values read from the heap with `load` have type `any`.

```casa
10 alloc = buffer
42 buffer store
buffer load         # type: any
```

## Type Casting

The `(TypeName)` syntax casts the top of the stack to the given type. This is a compile-time annotation only — no runtime check is performed.

**Stack effect:** `a -> TypeName`

```casa
buffer load (int)    # cast any -> int
```

This is useful after `load` or when working with generic data structures that return `any`.

## Comments

Line comments start with `#` and extend to the end of the line.

```casa
# This is a comment
42 print  # This is also a comment
```

There are no block comments.
