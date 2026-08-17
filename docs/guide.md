# Casa Guide

This guide assumes that you know common programming concepts such as values,
functions, variables, and loops. It does not assume experience with stack-based
languages.

## Values form a stack

Casa evaluates values from left to right. A literal puts a value on the value
stack. An operation consumes the values it needs and puts its result back.

```casa
3 4 + 2 * print
```

The expression evaluates as follows:

```text
source    stack
3         [3]
4         [3, 4]
+         [7]
2         [7, 2]
*         [14]
print     []
```

Casa does not need operator precedence or parentheses for this expression.

## Read stack effects

A stack effect states the input and output types of an operation. Inputs are
listed from the topmost consumed value downward. Outputs are listed in push
order.

```text
print: [T: Display] T -> None
dup:   T -> T T
+:     T T -> T
```

`None` means that the operation has no stack input or output. It is not the
`Option::None` value.

Most functions use the topmost value as their first argument. Arithmetic is the
explicit exception: `10 3 -` means `10 - 3`.

Comparison follows normal function operand order. The topmost value is the left
operand:

```casa
0 1 > print
```

This prints `true` because the expression means `1 > 0`.

## Define and call functions

A function declaration names its parameters in the order they are consumed.
The first parameter receives the topmost value.

```casa
fn subtract left:i64 right:i64 -> i64 {
    left right -
}

3 10 subtract print
```

The call pushes `3`, then `10`. The topmost value `10` becomes `left`, and `3`
becomes `right`. The function prints `7`.

Functions can also use unnamed stack inputs when a local name adds no clarity:

```casa
fn double i64 -> i64 {
    2 *
}

21 double print
```

Use named parameters when the name makes the function easier to understand.

## Bind values

Assignment removes the top value from the stack and binds it to a name:

```casa
42 = answer
answer print
```

The compiler infers the binding type. Add an annotation when inference needs
help:

```casa
import "std"

Option::None = missing:Option[i64]
missing drop
```

Pass `-L lib` when a program uses module-style imports from this repository.

## Branch on values

Conditions leave a `bool` on the stack. `then` consumes it. Every continuing
branch must leave the same stack effect.

```casa
fn rating score:i64 -> str {
    if 90 score >= then
        "excellent"
    elif 75 score >= then
        "good"
    else
        "keep practicing"
    fi
}

82 rating print
```

Remember that comparison uses the topmost value as its left operand. Therefore,
`90 score >=` means `score >= 90`.

## Compose a program

The following program combines an import, a typed function, an array, a binding,
a loop, method syntax, and string interpolation:

```casa
import "std"

fn rating score:i64 -> str {
    if 90 score >= then
        "excellent"
    elif 75 score >= then
        "good"
    else
        "keep practicing"
    fi
}

[72, 95, 81] = scores
for score in scores.iter do
    score rating = label
    f"{score}: {label}" println
done
```

Save it as `ratings.casa`, then compile and run it:

```sh
./casac ratings.casa -L lib -r
```

The output is:

```text
72: keep practicing
95: excellent
81: good
```

## Continue learning

- [Types and Literals](types-and-literals.md) describes Casa's types and values.
- [Functions and Lambdas](functions-and-lambdas.md) covers closures and function
  values.
- [Control Flow](control-flow.md) covers loops and exhaustive matching.
- [Structs and Methods](structs-and-methods.md) and [Enums](enums.md) cover custom
  data types.
- [Standard Library](standard-library.md) and [Collections](collections.md) cover
  reusable data structures and iterator operations.
