# List mutable iteration lends one element at a time

List traversal has three explicit ownership modes. `iter` borrows elements
shared, `iter_mut` lends an exclusive element borrow, and `into_iter` transfers
owned elements. Each follows original index order without requiring `Copy` or
`Clone`. This amends [ADR-0157](0157-collection-ownership-uses-two-explicit-mode-triads.md).

`ListIterMut[T]` retains an exclusive list borrow. Its `next` method borrows the
cursor exclusively and returns `Option[mut$T]`. Each result keeps the complete
cursor loaned until its last use. No other result can be obtained during that
loan. The list remains the sole element owner.

The cursor has inherent `all`, `any`, `count`, `find`, and `next` methods.
Predicates receive shared element borrows. `find` lends its result through the
cursor. The cursor does not implement `Iterable[mut$T]`: existing trait
defaults can retain a yield while advancing. Supporting simultaneous mutable
yields would require proofs of disjoint dynamic elements beyond the complete
input rule in [ADR-0108](0108-opaque-returned-borrows-keep-the-complete-input-loaned.md).

`for` uses an inherent or trait `next` method returning `Option`. It stores its
source once and destroys an owned iterator before execution continues after
exhaustion or `break`. A borrowed source remains available to its caller.
Lending results must end before the next iteration.

`into_iter` reverses its owned list once and captures it in a repeatable closure.
Each invocation pops one element. The capture owns the remainder and destroys
it when the iterator is dropped. This uses existing list ownership and closure
cleanup without another storage abstraction. Construction is linear and each
advance takes constant work.

## Repeatable callback boundary

A repeatable closure cannot return a reference through a retained exclusive
capture, even when that result is shared or wrapped in an aggregate. This
amends [ADR-0043](0043-all-closures-are-repeatable.md). The generic `fn[...]`
contract supplies independent results and has no invocation lifetime parameter.
A mutable capture reborrow hidden behind `fn[-> T]` would invalidate generic
consumers that keep an earlier `T` while calling again. Conditional loans would
require a change to symbolic generic checking and higher-order contracts.

Copied shared payloads, borrows of explicit call arguments, and ownership
transfers from captured collections remain valid. Named lending methods expose
the receiver borrow in their signature. Receiver reborrows survive aggregate
results and the returned-source inference from
[ADR-0175](0175-returned-borrow-sources-are-inferred-from-checked-bodies.md).

## Generic contract conformance

A trait implementation cannot hide a storage reborrow inside a return type
parameter whose abstract contract does not retain that loan. Conformance uses
the checked source summary and matches input positions. Explicit borrowed
results permit their stated capability. Stored payload transfers remain valid.

Generic callback signatures and callback fields follow the same rule before
substitution erases the abstract shape. Specializing `fn[mut$I -> T]` with a
borrowed `T` would let the callback lend input storage without requiring the
generic body to keep that input loaned. Explicit `fn[...]` contracts use their
conservative sources, including when a particular target is known.
