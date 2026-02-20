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

### `UNEXPECTED_TOKEN`

The parser expected one token but found another.

```casa
struct Foo { x int }
```

```
error[UNEXPECTED_TOKEN]: Expected `:` but got `int`
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

Branches of a conditional or loop leave the stack in inconsistent states.

```casa
if true then 1 else "two" fi
```

```
error[STACK_MISMATCH]: Stack state changed across branch: expected [...] but got [...]
```

### `SIGNATURE_MISMATCH`

A function's declared signature does not match the inferred signature from its body.

```casa
fn bad a:int -> str { a 1 + }
bad
```

```
error[SIGNATURE_MISMATCH]: Invalid signature for function `bad`: expected a:int -> str but inferred any -> int
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

## Multi-Error Collection

The compiler collects as many errors as possible within each compilation phase before stopping. For example, the identifier resolution phase will report all undefined names at once rather than stopping at the first one.

Within a single function, type checking stops after the first error because the stack state becomes unreliable. However, errors across different functions are collected independently.
