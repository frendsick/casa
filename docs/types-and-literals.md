# Types and Literals

Casa is a statically typed language. Types are checked at compile time and most can be inferred automatically — you rarely need to write type annotations outside of function signatures.

## Primitive Types

### `int`

Integer type. All integers are 64-bit signed values.

```casa
42 print       # 42
0 print        # 0
```

There are no negative integer literals. To get a negative number, subtract from zero:

```casa
0 1 - print    # -1
```

### `bool`

Boolean type with two values: `true` and `false`.

```casa
true print     # 1
false print    # 0
```

Booleans are printed as integers (`1` for true, `0` for false).

### `str`

String type. String literals are enclosed in double quotes.

```casa
"Hello world!" print
```

Strings support the following escape sequences: `\n` (newline), `\t` (tab), `\\` (backslash), `\"` (quote), `\0` (null).

## Composite Types

### `ptr`

Heap pointer returned by `alloc`. Used with `load` and `store` for heap memory access.

```casa
10 alloc = buffer    # allocate 10 heap slots
42 buffer store      # store 42 at buffer
buffer load print    # 42
```

Pointer arithmetic is supported with `+` and `-`:

```casa
10 alloc = buffer
99 buffer 3 + store   # store 99 at offset 3
buffer 3 + load print  # 99
```

See [Functions and Lambdas — Memory Intrinsics](functions-and-lambdas.md#memory-intrinsics) for details.

### `array`

Fixed-size array literal. Items must be literal values (integers, booleans, or strings).

```casa
[1, 2, 3]
```

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

## The `any` Type

`any` is a special wildcard type that matches any other type. It is used as an escape hatch when the type system cannot determine a precise type — for example, values read from the heap with `load` have type `any`.

```casa
10 alloc = buffer
42 buffer store
buffer load         # type: any
```

## Type Casting

The `(TypeName)` syntax casts the top of the stack to the given type. This is a compile-time annotation only — no runtime check is performed.

**Stack effect:** `( a -- TypeName )`

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
