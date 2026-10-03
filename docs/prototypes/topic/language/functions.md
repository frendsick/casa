# Functions

A named function consumes the values declared as its inputs and pushes its
declared outputs. Parameters are listed in consumption order, starting with
the topmost value.

## Operand order

| Source | Conventional expression | Result |
|---|---|---|
| `3 10 subtract` | `subtract(10, 3)` | `7`, for the definition below |
| `10 3 -` | `10 - 3` | `7` |
| `0 1 >` | `1 > 0` | `true` |

Arithmetic uses left-to-right operand order. Comparisons follow the function
convention. Source evaluation still proceeds left to right in each case.

## Declarations and calls

```casa
fn subtract left:i64 right:i64 -> i64 { left right - }

3 10 subtract print    # 7
```

The declaration is `fn subtract left:i64 right:i64 -> i64`. Its named
parameters explain the call: `10` becomes `left`, and `3` becomes `right`.
The complete example can be saved as `sample.casa` and run from the repository
root with `./casac sample.casa -r`.

Omit `->` when a function produces no output. List multiple output types after
`->` when a function produces multiple values. Outputs are pushed in the order
written, so the last output is on top. Every returning path must produce the
declared outputs.

## Stack effects

A stack effect contains types without parameter names. `subtract` has the
effect `i64 i64 -> i64`. This states how many values the function consumes and
produces, but the names in the declaration explain which input is `left`.

Inputs are listed from the topmost value downward. Outputs are listed in push
order. `None` denotes an empty side of a documented stack effect. It is not
an `Option::None` value and is not written as a function's return type.

A function value includes its effect in its type. For this example,
`&subtract` has type `fn[i64 i64 -> i64]`.

Stack snapshots use a different display convention: the top is on the right.
Immediately before `subtract` runs, the stack is `[3, 10]`. The first consumed
value is therefore the rightmost one.

## Borrowed parameters

`$T` requests shared access. `mut$T` requests exclusive mutable access. A plain
`T` parameter consumes the value. A non-`Copy` owner then moves to the callee.

Calls borrow available owners automatically when the parameter requires it.
See [List.push](../library/list.md#push) for a method that borrows its receiver
and consumes another value.

This sample covers calls and notation. The existing
[function reference](../../../functions-and-lambdas.md) contains the remaining
rules for returns, closures, and unsafe functions.
