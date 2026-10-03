# Casa library manual

Import `std` for the list operations in this sample. Use `std::` for names from
the module. Receiver calls such as `numbers.push` need no module prefix.

- [Create and change a list](#create-and-change-a-list)
- [List operations](#list-operations)
- [Read before mutation](#read-before-mutation)

## Create and change a list

`std::List[T]` owns a growable sequence of `T` values. Save this program as
`sample.casa`:

```casa
import "std"

[10, 20] std::List::from_array = numbers
30 numbers.push
0 numbers.get print "\n" print
numbers.pop print "\n" print
```

Run `./casac sample.casa -L lib -r` from the repository root. The `-L lib`
option makes the standard library available for module lookup.

Output:

```text
10
30
```

`push` adds the last value. `get` borrows the first. `pop` removes and returns
the last. After these calls the list contains `[10, 20]`.

## List operations

Declarations below are excerpts from `impl List` in
[lib/std.casa](../../../lib/std.casa). They use local module names and omit
the bodies. Call forms assume the `numbers` binding above.

| Declaration | Call | Contract |
|---|---|---|
| `pub fn push [T] self:mut$List[T] item:T` | `item numbers.push` | Append an item, borrowing the list exclusively and moving a non-`Copy` item into it |
| `pub fn get [T] self:$List[T] n:u64 -> $T` | `index numbers.get` | Borrow the indexed element. An out-of-range index terminates the program |
| `pub fn pop [T] self:mut$List[T] -> T` | `numbers.pop` | Remove and return the last element. An empty list terminates the program |

`push` produces no output. `get` keeps the list as the element's owner.
`pop` transfers ownership of the removed element when `T` is an owning type.
All three leave the caller's list binding available, subject to any returned
borrow.

Indices start at zero. `get` requires `n < length` and returns a borrow
directly, without an `Option`. `pop` requires a nonempty list.

The [handbook](handbook.md#ownership-at-calls) explains the input types.
Declarations already include input and output types, so this table does not
repeat them as separate stack effects.

## Read before mutation

A borrow returned by `get` keeps the list loaned until its last use. In the
example, `print` finishes using the element before `pop` changes the list.
Mutating the list while that borrow remains in use is rejected by the compiler.

Use `copy` to obtain a `Copy` element by value, or `.clone` when the type
implements `Clone` and an independent value is needed. Use `get_mut` to update
an element through an exclusive borrow.

The existing [collection reference](../../collections.md#lists) documents the
remaining operations beyond this sample. Complete programs are indexed in
[examples](../../../examples/README.md).
