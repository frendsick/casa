# Understand Compiler Diagnostics

Start with the first diagnostic in your source file. Later diagnostics can be
caused by the same problem.

```text
error[TYPE_MISMATCH]: Type mismatch
  --> program.casa:7:12
  |
7 | "text" consume_number
  |        ^^^^^^^^^^^^^^
  Expected: i64
  Got: str
```

Read it in this order:

1. `program.casa:7:12` identifies the file, line, and column.
2. The carets identify the operation that detected the problem.
3. `Expected` is what the operation needs at that stack position.
4. `Got` is the value that was present.

The error code in brackets is useful for searching this reference, but the
message and type details usually give the fix.

## Fix stack and type errors

A stack effect lists consumed values from the top of the stack downward:

```text
consume_number: i64 -> None
```

For a `TYPE_MISMATCH`, trace the values before the highlighted operation. Check
which value is on top, its type, and the function's parameter order. Remember
that a function's first parameter receives the topmost value.

`STACK_UNDERFLOW` means that an operation needs a value that is not present.
Look for a missing literal, an earlier operation that consumed the value, or a
branch that does not produce it.

`STACK_MISMATCH` means that continuing branches leave different stack states.
Trace each `if`, `match`, or loop path from the same starting stack. Every path
that continues must leave the same number and types of values.

`SIGNATURE_MISMATCH` means that a function body does not produce its declared
stack effect. Compare the declared inputs and outputs with every `return` and
with the end of the function body.

See [Casa Guide](guide.md#read-stack-effects) for stack-effect notation and
[Functions and Lambdas](functions-and-lambdas.md#declare-and-call-a-function)
for parameter order.

## Expected, got, and inferred types

Some diagnostics use `Inferred` instead of `Got`. `Inferred` describes the
stack effect or type calculated from the body. The same rule applies: compare
it with `Expected`, then find the first position where they differ.

An integer or floating-point literal can also need more context. Add a type
annotation when the surrounding operation cannot select a concrete width:

```casa
0 = offset:u64
```

## Notes and related locations

A diagnostic can include one or more `Note` sections. A note points to a related
declaration, branch, or value origin. The main location shows where the compiler
detected the error. The note helps identify where the conflicting value or rule
came from.

## Cascade errors

The compiler can continue after some failures so that it can report more than
one problem. A missing stack value is represented internally as `<missing>`.
If a later diagnostic contains `<missing>`, fix the earlier underflow first.
The later diagnostic will often disappear.

The compiler can also report independent errors from different functions or
compilation phases. After each edit, compile again and start with the earliest
remaining error in your code.

## Error-code reference

| Code | Meaning |
|---|---|
| `SYNTAX` | Invalid source form or unsupported construct |
| `UNEXPECTED_TOKEN` | A different token was required |
| `UNDEFINED_NAME` | A name cannot be resolved |
| `UNDEFINED_GLOBAL` | A `global` declaration does not name a global binding |
| `DUPLICATE_NAME` | A name or declaration is repeated |
| `INVALID_SCOPE` | A construct appears in a scope where it is not allowed |
| `TYPE_MISMATCH` | A value has the wrong type |
| `STACK_UNDERFLOW` | An operation does not have enough input values |
| `STACK_MISMATCH` | Control-flow paths leave incompatible stacks |
| `SIGNATURE_MISMATCH` | A function body does not match its declared stack effect |
| `INVALID_VARIABLE` | A binding or assignment is invalid |
| `UNMATCHED_BLOCK` | A block is missing its matching keyword |
| `MISSING_TRAIT_METHOD` | A type does not satisfy a required trait |
| `TRAIT_SIGNATURE_MISMATCH` | A trait declaration or implementation is incompatible |

The CLI recognizes `UNUSED_PARAMETER` and `LOSSY_TYPE_ANNOTATION` warning codes.
Warnings do not stop compilation.

Use the [language server](language-server.md) to see compiler diagnostics in an
editor.
