# List reference

`std::List[T]` owns a growable sequence of `T` values. Import `std` to name the
type. The declarations below use the local names from
[lib/std.casa](../../../../lib/std.casa) and omit their bodies.

Each entry includes a declaration, a type-only stack effect, a call form, and
the observable contract. See [notation](notation.md) for their differences.
Call forms are fragments that assume a `numbers` binding.

## push

```text
pub fn push [T] self:mut$List[T] item:T
```

Stack effect: `mut$List[T] T -> None`.

Call: `item numbers.push`.

Appends `item` to the end of the list. The call borrows the list exclusively.
A non-`Copy` item moves into the list, which becomes its owner. The caller
keeps the list binding. The method produces no output.

The list must be available for an exclusive borrow. In particular, a value
previously borrowed from it cannot remain in use across this mutation.

## get

```text
pub fn get [T] self:$List[T] n:u64 -> $T
```

Stack effect: `$List[T] u64 -> $T`.

Call: `index numbers.get`.

Borrows the element at zero-based index `n`. The list keeps ownership of the
element and cannot be mutated while the returned borrow remains in use. The
call does not remove or clone the element.

The index must be less than the list's length. An out-of-range index terminates
the program. The method does not return an `Option`.

## pop

```text
pub fn pop [T] self:mut$List[T] -> T
```

Stack effect: `mut$List[T] -> T`.

Call: `numbers.pop`.

Removes the final element and returns it by value. The caller receives its
ownership when `T` is an owning type. The list is borrowed exclusively during
the call and remains available afterward.

The list must be nonempty. Popping an empty list terminates the program.

## Use these operations together

[Change a list](../how-to/change-a-list.md) contains a complete runnable example
and explains when an element borrow must end. The existing
[collection reference](../../../collections.md#lists) covers the remaining
methods beyond this prototype's three entries.
