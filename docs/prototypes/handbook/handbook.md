# Casa handbook

Casa is a statically typed, stack-based language for Linux. This sample chapter
covers values, calls, and the notation used in the library manual.

- [Values and the stack](#values-and-the-stack)
- [Call a function](#call-a-function)
- [Operand order](#operand-order)
- [Stack effects](#stack-effects)
- [Ownership at calls](#ownership-at-calls)

## Values and the stack

Source runs from left to right. A literal pushes a value. An operation consumes
the values it needs and can push results.

```casa
3 4 + 2 * print    # 14
```

Save this complete program as `sample.casa` and run `./casac sample.casa -r`
from the repository root. The stack progresses through `[3]`, `[3, 4]`, `[7]`,
`[7, 2]`, `[14]`, and `[]`. The top is shown on the right. No operator
precedence is needed.

## Call a function

Parameters are listed in consumption order. The first parameter receives the
topmost value.

```casa
fn subtract left:i64 right:i64 -> i64 { left right - }

3 10 subtract print    # 7
```

This is another complete program, run with the same command. `left` receives
`10`, and `right` receives `3`. A declaration gives the function's name,
parameters, and output types. Its definition also includes the body.

Omit `->` for a function with no output. Multiple output types describe values
in push order. The last output becomes the topmost value. Every returning path
must produce the declared outputs.

## Operand order

| Operation | Casa | Conventional reading |
|---|---|---|
| Function | `3 10 subtract` | `subtract(10, 3)` |
| Arithmetic | `10 3 -` | `10 - 3` |
| Comparison | `0 1 >` | `1 > 0` |
| Method | `30 numbers.push` | `push(numbers, 30)` |

Comparisons follow function operand order. Arithmetic is the exception. For a
method, the receiver is the first consumed parameter.

## Stack effects

A stack effect gives input and output types. Its inputs use consumption order,
starting at the top. Its outputs use push order. This differs from a stack
snapshot, which shows the current values with the top on the right.

| Operation | Stack effect |
|---|---|
| The `subtract` function above | `i64 i64 -> i64` |
| Integer subtraction | `i64 i64 -> i64` |
| Integer greater-than comparison | `i64 i64 -> bool` |
| Integer printing | `i64 -> None` |

The types alone do not show operand order or parameter names. Use the
[operand-order table](#operand-order) to read the expressions.

`None` denotes an empty side of an effect. It is not an `Option::None` value
or a return type to write after `->`. A function value wraps its effect in
`fn[...]`. For example, `&subtract` has type `fn[i64 i64 -> i64]`.

## Ownership at calls

| Input type | Access |
|---|---|
| `T` | Consume a value, moving it when it is a non-`Copy` owner |
| `$T` | Borrow for shared access |
| `mut$T` | Borrow for exclusive mutable access |

Calls borrow available owners automatically when their parameter types require
it. A borrow returned from a call keeps its source loaned until the borrow's
last use. Finish that use before mutating the source.

The [library manual](library.md) applies these rules to a list. For language
topics outside this sample, use the current
[Casa guide](../../guide.md).
