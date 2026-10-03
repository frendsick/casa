# Ownership and Borrows

An owner is responsible for a value's lifetime. A borrow permits access while
that owner remains responsible for cleanup.

- [Move a value](#move-a-value)
- [Borrow for a call](#borrow-for-a-call)
- [Reborrow without moving](#reborrow-without-moving)
- [Return a borrow](#return-a-borrow)
- [Borrow disjoint fields](#borrow-disjoint-fields)
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
struct Person { age: i64 }

fn replace_both left:mut$Person right:mut$Person { }

36 Person = person
# person person replace_both  # Error: the exclusive arguments alias.
```

## Return a borrow

A returned borrow keeps each compatible borrowed input loaned until its last
use. The caller cannot know which input supplied an opaque result:

```casa
struct Person { age: i64 }

fn select first:$Person second:$Person choose_first:bool -> $Person {
    if choose_first then first else second fi
}

36 Person = person
40 Person = other
true other person select = selected
# person drop       # Error: selected can borrow person.
selected.age print
person drop
other drop
```

A function cannot return a borrow of a local owner. The diagnostic identifies
the local owner that would escape.

## Borrow disjoint fields

A function can return multiple exclusive field borrows when their named paths
do not overlap:

```casa
struct Item { value: i64 }
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
paths are not part of a public function type.

## Cleanup and captured values

Casa destroys remaining owners when their scope ends normally. Moving an owner
transfers its cleanup responsibility. A borrow does not become a second owner.
See [Custom destruction](structs-and-methods.md#custom-destruction) for the
cleanup order and reserved `drop` method. `panic` and `process::exit` terminate
without cleanup.

Structs and enums can retain borrows in their fields. Their loans last through
any cleanup that can observe those borrows. See
[Structs and Methods](structs-and-methods.md#receiver-access-and-stored-borrows) for the
aggregate rules. [Closures](functions-and-lambdas.md#lambdas-and-closures)
describe borrowed and moved captures.

Collection references state which operations return borrows, move elements,
or destroy replaced values.
