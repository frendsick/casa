# Ownership and Borrows

An owner is responsible for a value's lifetime. A borrow permits access while
that owner remains responsible for cleanup.

- [Move a value](#move-a-value)
- [Borrow for a call](#borrow-for-a-call)
- [Reborrow without moving](#reborrow-without-moving)
- [Return a borrow](#return-a-borrow)
- [Borrow disjoint fields](#borrow-disjoint-fields)
- [Share an immutable value](#share-an-immutable-value)
- [Cleanup and captured values](#cleanup-and-captured-values)

## Move a value

A plain `T` parameter consumes an owned value. Non-Copy owners can be consumed
only once:

```casa
import "std"

fn consume text:std::String { text drop }

"one owner".to_str = text
text consume
# text consume    # Error: text was already moved.
```

## Borrow for a call

Use `$T` for shared access and `mut$T` for exclusive mutable access. Calls
borrow an available owner automatically:

```casa
import "std"

fn length text:$str -> u64 { text.length }

fn clear text:mut$std::String { text.clear }

"Casa".to_str = text
text.as_str length print
text clear
text.as_str length print
```

## Reborrow without moving

Shared borrows can be duplicated with `dup` and `over`, but they do not satisfy
`Copy` bounds. Exclusive borrows and non-Copy owners are affine. For owned
values, `dup` and the copied value of `over` require `Copy`. `swap` and `rot`
only move values, so they also work with non-Copy owners.

An owner or exclusive borrow can be reborrowed for a call. When the call does
not return a borrow, the reborrow ends when the call returns. One call cannot
borrow the same binding exclusively more than once or combine shared and
exclusive borrows of that binding:

```casa
struct Person {
    age: i64
}

fn replace_both left:mut$Person right:mut$Person { }

36 Person = person
# person person replace_both  # Error: the exclusive arguments alias.
```

## Return a borrow

A returned borrow keeps its possible source inputs loaned until its last use.
Casa infers those sources from every returning path in the function body.
Each output has its own source summary. An aggregate output keeps the union
of the dependencies of its contents. In this example, either input can supply
the result:

```casa
struct Person {
    age: i64
}

fn select first:$Person second:$Person choose_first:bool -> $Person {
    if choose_first then
        first
    else
        second
    fi
}
36 Person = person
40 Person = other
true other person select = selected
# person drop       # Error: selected can borrow person.
selected.age print
person drop
other drop
```

A function that always returns `first` keeps only that input loaned. The same
summary applies to `&function = callback` followed by `callback exec`, including
a function value moved to another binding. If a branch can select different
functions, the result retains all their possible sources.

An explicit type such as `= callback:fn[$Person $Person -> $Person]` uses a
general contract. It keeps both compatible inputs loaned even when the selected
function returns only the first. Function parameters and outputs with explicit
`fn[...]` types have the same limit. Calls checked only against trait requirements
also use conservative sources. A concrete implementation can use its checked
summary. Recursive call cycles retain conservative sources.

Inferred sources are part of the public borrowing contract. Adding a source in
a function body can break callers without changing its written signature.
Hovers show output sources by input position, with input 1 at the top of the
stack. Calls still keep each selected source loaned as a complete owner.
See [the lookup example](../examples/borrow_lookup.casa) for reuse of a temporary
key while a returned value remains live.

A function cannot return a borrow of a local owner. The diagnostic identifies
the local owner that would escape.

## Borrow disjoint fields

A function can return multiple exclusive field borrows when their named paths
do not overlap:

```casa
struct Item {
    value: i64
}

struct Pair {
    left:  Item
    right: Item
}

fn split pair:mut$Pair -> mut$Item mut$Item {
    pair.left
    pair.right
}
```

The compiler rejects duplicate or nested overlapping outputs. After a call,
the returned borrows keep the complete borrowed input loaned because field
paths are not part of a public [function type](notation.md#function-types).

## Share an immutable value

`std::Shared[T]` lets several owners share one immutable `T`. `Shared::new`
moves the value into a heap allocation. `clone` returns another handle to the
same value without cloning `T`. `get` borrows the value while the handle
remains live. The last handle to be destroyed destroys the value:

```casa
import "std"

"settings".to_str std::Shared::new = first
first.clone = second
first drop
second.get.as_str print # The value survives until second is destroyed.
```

The shared value cannot be mutated or moved out. It is complete before
`Shared::new` takes it, so safe code cannot make it refer to its own handle.
Use one owner plus IDs or indices for cyclic or mutable graphs.

Equality, ordering, hashing, and display use the shared value's
implementations when `T` implements those traits. Two handles to equal
separate values compare equal. `Clone` does not require `T: Clone`.

Reference counts are not atomic because Casa programs run on one thread.
`Shared[T]` does not retain the borrow origins of a `T` that contains borrows.
Store owned values in it.

## Cleanup and captured values

Casa destroys remaining owners when their scope ends normally. Moving an owner
transfers its cleanup responsibility. A borrow does not become a second owner.
See [Custom destruction](structs-and-methods.md#custom-destruction) for the
cleanup order and reserved `drop` method. `panic` and `std::process::exit` terminate
without cleanup.

Structs and enums can retain borrows in their fields. Their loans last through
any cleanup that can observe those borrows. See
[Structs and Methods](structs-and-methods.md#receiver-access-and-stored-borrows) for the
aggregate rules. [Closures](functions-and-lambdas.md#lambdas-and-closures)
describe borrowed and moved captures.

Collection references state which operations return borrows, move elements,
or destroy replaced values.
