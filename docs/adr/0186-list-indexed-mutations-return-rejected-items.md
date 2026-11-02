# List indexed mutations return rejected items

`List.set`, `List.replace`, and `List.insert` return the supplied item in
`Result::Error` when the index is out of range. Failure leaves the list
unchanged and transfers ownership of the item back to the caller. Success
preserves each operation's mutation and ownership behavior. `insert` accepts
an index equal to the list length.

These operations consume an item. Returning `Option::None` on failure would
destroy that item before the caller could recover it. Returning the item in
the error preserves ownership without a separate check or new mutation API.
Keeping termination as a precondition would prevent recovery from an invalid
index. This implements the decision in
[#1035](https://github.com/frendsick/casa/issues/1035).

`set` and `insert` return `Result[Unit T]`. `replace` returns `Result[T T]`,
with the previous element in `Ok` and the rejected item in `Error`. The two
arms have the same payload type, so callers must distinguish their meaning.
No `Copy`, `Clone`, or display bound is required. Callers that require a valid
index can use `unwrap`. Propagating failure with `?` requires a compatible
error type, or an explicit conversion with `map_error`.

Call forms, bounds, and ownership rules are documented in
[Lists](../lists.md).
