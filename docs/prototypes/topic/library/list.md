# List

`std::List[T]` owns a growable sequence of `T` values. Use it when elements must
be added or removed. Import `std`, then qualify constructors with `std::`.

## Create and use a list

Save this complete example as `sample.casa`. Run
`./casac sample.casa -L lib -r` from the repository root.

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

## Operations

These are declaration excerpts from `impl List` in
[lib/std.casa](../../../../lib/std.casa). `List` uses its local module spelling
in the declarations. Use `std::List` when naming it from the importing program.

### push

```text
pub fn push [T] self:mut$List[T] item:T
```

Appends `item`. The receiver is borrowed exclusively and remains owned by the
caller. A non-`Copy` item moves into the list. The method produces no result.

Call form: `item numbers.push`. Its stack effect is
`mut$List[T] T -> None`, with the receiver consumed first. See
[stack effects](../language/functions.md#stack-effects) for the notation.

### get

```text
pub fn get [T] self:$List[T] n:u64 -> $T
```

Returns a shared borrow of the element at zero-based index `n`. The list keeps
ownership of the element. An index greater than or equal to the list's length
terminates the program.

Call form: `index numbers.get`. Its stack effect is
`$List[T] u64 -> $T`.

### pop

```text
pub fn pop [T] self:mut$List[T] -> T
```

Removes the last element and returns it by value. The caller receives ownership
when `T` is an owning type. Popping an empty list terminates the program.

Call form: `numbers.pop`. The receiver remains available after the call.

## Borrowing elements

A value returned by `get` keeps the list loaned until that borrow's last use.
Finish using it before mutating the list. In the complete example, the first
`print` uses the borrowed element before `pop` changes the list.

Use `get_mut` for exclusive access to an element. Use `.clone` when an owned
copy is needed and `T` implements `Clone`. Further operations are in the
existing [collection reference](../../../collections.md#lists).
