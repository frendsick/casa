# The stack and calls

Casa evaluates source from left to right. Values go onto a stack. Operations
consume values from its top. This removes the need for operator precedence,
but it makes operand order part of reading a call.

## Follow one call

This complete program prints `7`:

```casa
fn subtract left:i64 right:i64 -> i64 { left right - }

3 10 subtract print
```

Save it as `sample.casa` and run `./casac sample.casa -r` from the repository
root.

| After | Stack, with the top on the right | What happened |
|---|---|---|
| `3` | `[3]` | A literal pushes a value |
| `10` | `[3, 10]` | The second literal becomes the top value |
| `subtract` | `[7]` | `left` receives `10`, `right` receives `3`, and the result is pushed |
| `print` | `[]` | The result is printed and consumed |

The first declared parameter receives the topmost argument. For a method call,
that parameter is the receiver. In `30 numbers.push`, the item is therefore
below the receiver when the method consumes its inputs.

## Arithmetic and comparisons

Arithmetic is the explicit exception to function operand order. `10 3 -`
means `10 - 3`. This rule applies to the subtraction inside `subtract` too.

Comparison uses the function convention: `0 1 >` means `1 > 0`. When checking
an index against a length, `length index <` means `index < length`.

Operand order and source evaluation order describe different things. Reading
source left to right does not make the first written value the first function
argument.

## Types do not preserve parameter names

The effect `i64 i64 -> i64` says that two integers are consumed and one is
produced. It cannot distinguish subtraction's `left` from `right`. A named
declaration and a concrete call supply that information.

The [notation reference](../reference/notation.md) defines how stack effects,
declarations, and function types are written. The
[tutorial](../tutorials/functions-and-lists.md) applies these rules in a complete
program with a list.
