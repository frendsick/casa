# Add, read, and remove a list element

Use `push` to append, `get` to read an element through a borrow, and `pop` to
remove the last element. This page assumes you already know how Casa calls
consume arguments.

## Complete example

Save this as `sample.casa` and run `./casac sample.casa -L lib -r` from the
repository root:

```casa
import "std"

[10, 20] std::List::from_array = numbers
30 numbers.push
0 numbers.get print "\n" print
numbers.pop print "\n" print
```

Output:

```text
10
30
```

The list contains `[10, 20]` after the last line. `push` and `pop` borrow it
exclusively for their calls, so the binding remains available afterward.

## Finish reading before changing the list

`get` returns `$T`, a shared borrow of the element. A later mutation requires
that borrow's last use to have finished. The example prints the borrowed value
before calling `pop`.

If a value must remain available independently of the list, clone the borrowed
element when its type implements `Clone`. For a `Copy` element, use `copy` to
obtain the value. See the [get contract](../reference/list.md#get).

## Check preconditions for variable input

The example uses index `0` in a nonempty list and pops after an append. For
variable inputs, require `index < length` before calling `get` and check that
the list is nonempty before calling `pop`. An invalid index or an empty pop
terminates the program.

In Casa, write the comparison as `numbers.length index <`. The topmost
operand, `index`, is the left side of `<`. See
[operand order](../explanation/stack-and-calls.md#arithmetic-and-comparisons)
if this form is unfamiliar.
