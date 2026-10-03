# Call functions and use a list

Casa evaluates values from left to right. Each value goes onto the stack. A
function consumes its arguments from the top and can push results back.

## Call a function

Save this complete program as `sample.casa` in the repository root:

```casa
fn subtract left:i64 right:i64 -> i64 { left right - }

3 10 subtract print
```

Run it from the repository root:

```sh
./casac sample.casa -r
```

It prints `7`. The call puts `3` on the stack, then `10`. The first parameter,
`left`, receives the topmost value, `10`. The second parameter receives `3`.
Inside the function, arithmetic reads left to right: `left right -` means
`left - right`.

Functions and comparisons use the topmost value as their first operand.
Arithmetic is the exception. See [operand order](language/functions.md#operand-order)
for a comparison of these forms.

## Change a list

A `List[T]` owns a growable sequence of values. Import the standard library and
use its namespace when naming the type or a constructor. Receiver calls such as
`numbers.push` use the receiver's type to find the method.

Replace `sample.casa` with this complete program:

```casa
import "std"

[10, 20] std::List::from_array = numbers
30 numbers.push
0 numbers.get print "\n" print
numbers.pop print "\n" print
```

The library search path is needed for `import "std"`:

```sh
./casac sample.casa -L lib -r
```

Output:

```text
10
30
```

`push` appends `30`. `get` borrows the element at index `0`, and `print` uses
that borrow before the next line. `pop` removes and returns the last element.
The list still contains `10` and `20` after the program prints `30`.

Reading an index outside the list or popping an empty list terminates the
program. The [List reference](library/list.md) gives each operation's contract.
