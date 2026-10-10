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
dup:   [T: Copy] T -> T T
+:     T T -> T
```

`None` means that the corresponding side of the stack effect is empty. It is
not the `Option::None` value.

Named functions use the topmost value as their first argument. Binary symbolic
operators use source order: `10 3 -` means `10 - 3`, and `0 1 <` means `0 < 1`:

```casa
0 1 < print
```

This prints `true` because the expression means `0 < 1`. A stack snapshot
shows the top on the right, while effect inputs start at the top. See
[Reference notation](notation.md) for signatures, effects, and function types.

## Define and call functions

A function signature names its parameters in the order they are consumed.
The first parameter receives the topmost value.

```casa
fn subtract left:i64 right:i64 -> i64 { left right - }

3 10 subtract print
```

The call pushes `3`, then `10`. The topmost value `10` becomes `left`, and `3`
becomes `right`. The function returns `7`, which `print` writes to standard output.

Functions can also use unnamed stack inputs when a local name adds no clarity:

```casa
fn double i64 -> i64 { 2 * }

21 double print
```

Use named parameters when the name makes the function easier to understand.

## Bind values

Assignment removes the top value from the stack and binds it to a name:

```casa
42 = answer
answer print
```

An owned value that does not implement `Copy` moves when an operation consumes
it. The source binding cannot be used again. A parameter of type `$T` borrows
an owned value without moving it:

```casa
import "std"

fn length text:$str -> u64 { text.length }

"Casa" = text
text length print
text print
```

A `mut$T` parameter can update an owner through an exclusive borrow. See
[Ownership and borrows](ownership.md) for
mutable borrow examples and the complete rules. Casa destroys each remaining
owner when its scope ends. See [Custom destruction](structs-and-methods.md#custom-destruction)
for cleanup methods and destruction order.

The compiler infers the binding type. Add an annotation to select or require a
specific type:

```casa
255 = byte: u8
byte print
```

Pass `-L lib` when a program uses module-style imports from this repository.

## Branch on values

Conditions leave a `bool` on the stack. `then` consumes it. Every continuing
branch must leave the same stack effect.

```casa
fn rating score:i64 -> str {
    if score 90 >= then
        "excellent"
    elif score 75 >= then
        "good"
    else
        "keep practicing"
    fi
}
82 rating print
```

`score 90 >=` means `score >= 90`.

## Try a complete program

This program adds an [array iterator](collections.md#arrays), a
[for loop](control-flow.md#for-loops),
[string interpolation](types-and-literals.md#string-interpolation), and
`std::println_string` from the standard library.

```casa
import "std"

fn rating score:i64 -> str {
    if score 90 >= then
        "excellent"
    elif score 75 >= then
        "good"
    else
        "keep practicing"
    fi
}
[72, 95, 81] = scores
for score in scores.iter do
    score rating = label
    f"{score}: {label}" std::println_string
done
```

Save it as `ratings.casa`. Then, compile and run it:

```sh
./casac ratings.casa -L lib -r
```

Output:

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
- [Optional Values and Errors](optional-values-and-errors.md) covers `Option`,
  `Result`, and `?`.
- [Collections](collections.md) covers reusable data structures and iterators.
- Use [README.md](../README.md#learn-casa) for the complete reference links.
