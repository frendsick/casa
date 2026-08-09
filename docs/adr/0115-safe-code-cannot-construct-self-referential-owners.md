# Safe code cannot construct self-referential owners

Safe code cannot construct an owned aggregate that contains both an owner and a borrow derived from that same owner:

```casa
struct TextView {
    text: str
    view: $str
}

"hello" = text
text borrow = view

TextView {
    text: text
    view: view
} # error: constructing the value moves borrowed text
```

The ordinary rule preventing movement of a borrowed owner rejects construction. Casa does not add pinning or multi-stage initialization to make this special case possible.

## Consequences

- A struct may still contain an owner and a borrow of some external owner; only an internal self-reference is forbidden.
- Moving an aggregate containing a borrow moves the borrow handle, not its external referent, and remains subject to the referent's origin lifetime.
- Parsers, cursors, and views over owned buffers store offsets or indices and derive temporary borrows when needed.
- Recursive ownership indirection does not imply stable user-observable addresses or permit self-reference.
- A pinned owner abstraction remains deferred until a concrete API cannot use offsets, indices, handles, or external ownership.
