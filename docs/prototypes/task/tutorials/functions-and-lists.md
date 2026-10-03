# Write a function and change a list

You will call a function, append its result to a list, and read the first and
last elements. This lesson assumes that you know variables and functions in
another language.

First [install Casa](../../../../README.md#install). Work from the repository
root so `./casac` and `lib` refer to the installed compiler and standard library.

## 1. Call a named function

Save this program as `sample.casa`:

```casa
fn subtract left:i64 right:i64 -> i64 { left right - }

3 10 subtract print
```

Run it:

```sh
./casac sample.casa -r
```

It prints `7`. The first parameter receives the topmost value, so `left` is
`10` and `right` is `3`. The arithmetic expression inside the function subtracts
`right` from `left`.

Before continuing, change the call to `10 3 subtract print`. It prints `-7`
because `left` now receives `3`.

## 2. Store a result in a list

Replace the file with this program:

```casa
import "std"

fn subtract left:i64 right:i64 -> i64 { left right - }

[10, 20] std::List::from_array = numbers
3 33 subtract = value
value numbers.push
0 numbers.get print "\n" print
numbers.pop print "\n" print
```

Run it with the standard library search path:

```sh
./casac sample.casa -L lib -r
```

Output:

```text
10
30
```

The import makes the library available under `std`. `std::List::from_array`
creates a list from the two array elements. The function call returns `30`,
which `push` appends. The receiver `numbers` is the first argument of the
method, so the item appears before it in the call.

`get` borrows the first element. `pop` removes and returns the last element.
The first `print` finishes using its borrow before the list changes.

## 3. Check the resulting list

Add this line at the end of the program:

```text
numbers.length print "\n" print
```

Run the same command again. The final line is `2`: one value was added and
then removed.

Read [the stack and calls](../explanation/stack-and-calls.md) to understand why
arithmetic and function calls use different operand orders. Use the
[List reference](../reference/list.md) to check which operations borrow, move,
or mutate values, and which calls can terminate the program.
