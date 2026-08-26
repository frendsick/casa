# Collection ownership uses two explicit mode triads
related issue: #369

This refines the collection consequences of
[ADR-0013](0013-affine-ownership-with-automatic-storage.md) and
[ADR-0014](0014-explicit-unsafe-boundary.md).

Casa keeps `get`, `get_mut`, and `remove` for checked element access, and
`iter`, `iter_mut`, and `into_iter` for traversal. The names stay familiar while
their return types expose the ownership mode. Renaming the methods to repeat
`borrow` or `own` adds words without adding information, while one overloaded
method would hide the ownership choice.

For `List[T]`, access returns `Option[$T]`, `Option[mut$T]`, or `Option[T]`.
For `Map[K V]`, the corresponding value returns have the same shapes.
`Map::iter` yields `Pair[$K $V]`, `Map::iter_mut` yields
`Pair[$K mut$V]`, and `Map::into_iter` yields `Pair[K V]`. Map keys never receive
mutable access while stored because changing equality or hashing could invalidate
their placement.

`Set[K]` keeps `has` for observation. Its `remove` returns `Option[K]`, `iter`
yields `$K`, and `into_iter` yields `K`. It has no `get_mut` or `iter_mut`.
Mutation that can change equality or hashing removes the owned value, changes it,
and inserts it again.

## Consequences

- Missing keys and out-of-range indexes return `Option::None`. A caller whose
  index is an established invariant uses `.unwrap`. Casa adds no parallel `at`
  or `at_mut` surface.
- `get_ref` is not a second List access mode. `get` already supplies the shared
  borrow.
- `iter_mut` keeps the complete source exclusively loaned and yields at most one
  live mutable element borrow at a time.
- `into_iter` consumes the source and moves each element exactly once. Destroying
  the iterator early destroys every remaining element exactly once.
- List traversal preserves index order. Map and Set traversal order remains
  unspecified.
- Entry-returning removal and key, value, or consuming projection iterators stay
  deferred until real callers justify the larger interface.
