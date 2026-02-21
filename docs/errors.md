# Errors

The Casa compiler provides Rust-style error diagnostics with source context. When errors are detected, the compiler shows the error kind, a description, the file location, and the relevant source line with carets highlighting the problematic span.

## Error Format

```
error[UNDEFINED_NAME]: Identifier `foo` is not defined
  --> examples/fizzbuzz.casa:15:5
   |
15 | foo
   | ^^^
```

Each error includes:

- **Error kind** in brackets (e.g. `UNDEFINED_NAME`)
- **Message** describing the problem
- **File path, line, and column** where the error occurs
- **Source line** with carets (`^^^`) underlining the relevant span

Some errors include additional context:

- **Expected** and **Got** (or **Inferred**) lines showing what was expected versus what was found
- **Notes** with secondary source annotations pointing to related locations (e.g. where a mismatched value was originally pushed, or which branches have incompatible stack effects)

When multiple errors are found, they are all printed before the compiler stops, followed by a summary line:

```
Found 3 error(s).
```

## Error Kinds

### `SYNTAX`

Malformed tokens or structural problems in the source code.

```casa
"unterminated string
```

```
error[SYNTAX]: Unclosed string literal
```

Invalid escape sequences in strings are also reported as `SYNTAX` errors:

```casa
"bad\q"
```

```
error[SYNTAX]: Invalid escape sequence `\q`
```

### `UNEXPECTED_TOKEN`

The parser expected one token but found another.

```casa
struct Foo { x int }
```

```
error[UNEXPECTED_TOKEN]: Unexpected token
  Expected: `:`
  Got: `int`
```

### `UNDEFINED_NAME`

An identifier is used but not defined as a function, variable, struct, or intrinsic.

```casa
foo bar baz
```

```
error[UNDEFINED_NAME]: Identifier `foo` is not defined
error[UNDEFINED_NAME]: Identifier `bar` is not defined
error[UNDEFINED_NAME]: Identifier `baz` is not defined
Found 3 error(s).
```

Multiple undefined names are collected and reported together.

### `DUPLICATE_NAME`

An identifier is defined more than once in a context where duplicates are not allowed.

```casa
fn foo { }
fn foo { }
```

```
error[DUPLICATE_NAME]: Identifier `foo` is already defined
```

### `INVALID_SCOPE`

A construct appears in a scope where it is not allowed (e.g. defining a function inside another function).

```casa
fn outer {
    fn inner { }
}
```

```
error[INVALID_SCOPE]: Functions should be defined in the global scope
```

This also applies to `impl` blocks and `struct` definitions:

```casa
fn outer {
    impl Foo { }
}
```

```
error[INVALID_SCOPE]: Implementation blocks should be defined in the global scope
```

### `TYPE_MISMATCH`

A type does not match what was expected.

```casa
fn bad[T] T T -> T T { }
42 "hi" bad
```

```
error[TYPE_MISMATCH]: Type variable `T` bound to `str` but got `int`
```

### `STACK_MISMATCH`

Branches of a conditional or loop leave the stack in inconsistent states. The error shows each branch's stack signature so you can see which branch diverges.

```casa
fn branchy bool -> int {
    if dup then
        1 2
    else
        3
    fi
}
```

```
error[STACK_MISMATCH]: Branches have incompatible stack effects
  --> examples/multi_error.casa:27:5
   |
27 |     fi
   |     ^^
  Note: `if` branch has signature `any -> any int int`
  --> examples/multi_error.casa:23:5
   |
23 |     if dup then
   |     ^^
  Note: `else` branch has signature `any -> any int`
  --> examples/multi_error.casa:25:5
   |
25 |     else
   |     ^^^^
```

### `SIGNATURE_MISMATCH`

A function's declared signature does not match the inferred signature from its body.

```casa
fn bad a:int -> str { a 1 + }
bad
```

```
error[SIGNATURE_MISMATCH]: Invalid signature for function `bad`
  Expected: int -> str
  Inferred: any -> int
```

### `INVALID_VARIABLE`

Attempting to assign a value of a different type to an existing variable.

```casa
42 = x
"hello" = x
```

```
error[INVALID_VARIABLE]: Cannot override global variable `x` of type `int` with other type `str`
```

### `UNMATCHED_BLOCK`

A block construct is missing its closing keyword or has mismatched block markers.

```casa
if true then
    42 print
```

```
error[UNMATCHED_BLOCK]: `if` without matching `fi`
```

### `DUP_OWNED`

Attempting to duplicate an owned type with `dup` or `over`. Use `clone` instead.

```casa
[1, 2, 3] dup
```

```
error[DUP_OWNED]: Cannot duplicate owned type `array[int]`, use `clone` instead
```

### `CAPTURE_OWNED`

Attempting to capture an owned type in a lambda closure.

```casa
[1, 2, 3] = arr
{ arr.length }
```

```
error[CAPTURE_OWNED]: Cannot capture owned type `array[int]` in closure
```

## Multi-Error Collection

The compiler collects as many errors as possible within each compilation phase before stopping. For example, the identifier resolution phase will report all undefined names at once rather than stopping at the first one.

Within a single function, type checking stops at the first error because the stack state becomes unreliable. However, when checking all functions, errors from different functions are collected and reported together.

## Warnings

The compiler also reports non-fatal warnings. Currently the only warning kind is:

### `UNUSED_PARAMETER`

A function parameter is declared but not used in the function body and is instead passed through the stack untouched.

```
warning[UNUSED_PARAMETER]: Unused parameter `int` in function `add`
```
