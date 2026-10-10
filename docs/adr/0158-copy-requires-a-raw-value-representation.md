# Copy requires a raw value representation
status: amended by [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md)
related issues: #478, #792

`Copy` is accepted only when duplicating the complete runtime value cannot
duplicate ownership or exclusive access, or require allocation. The compiler
checks the value representation before it checks fields and trait bounds.

The compiler integrates the canonical standard-library methodless Copy marker
with implicit reuse, `dup`, and `over`. An unrelated trait named Copy does not
gain that behavior. A freestanding `std` may provide the canonical declaration
under [ADR-0080](0080-language-traits-use-minimum-contracts.md). Its bounds and
supertraits use ordinary trait machinery.

Ordinary structs and payload enums can request `derives Copy`. Eligible concrete
instances store the complete value in automatic destination storage. A temporary
address can carry that body between operations, but copying duplicates the body.
Function results use destination storage owned by the caller, so returning a
Copy aggregate does not require a backing allocation.

Every owned field must satisfy Copy. Shared-borrow fields preserve their origins
under [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md).
Exclusive borrows, custom destruction, and ownership-bearing recursive
indirection remain ineligible. Generic Copy bodies use the concrete field
layouts. Generic instances that do not satisfy Copy retain their affine layout.
Ordinary layouts remain compiler-owned and have no C ABI guarantee.

Owned callable fields remain affine because a function value can own a capture
environment. A shared borrow of a callable can be stored in a Copy aggregate.

Extern structs retain their fixed C-layout body. Fixed arrays store their
elements directly and implement Copy when their element type implements Copy,
including zero-length arrays. Payload-free enums use a raw tag.

Escaping into a closure capture, an indirect owned field, or `ptr::into_raw`
requires storage with the destination's lifetime. That placement can allocate.
The Copy operation itself performs no allocation, user call, or destruction.
Typed raw reads and writes retain their ownership-transfer contract.

## Consequences

- User-defined structs and enums request Copy with `derives Copy` under
  [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md).
- Empty structs and ordinary structs containing Copy fields can derive Copy.
- `Option[T]` derives Copy when `T: Copy`. `Result[T E]` derives Copy when both
  payload types satisfy Copy.
- Shared borrows do not gain Copy conformance. A copied aggregate retains the
  origins of its stored shared borrows.
- Structural Clone remains explicit and calls each owned field's Clone method.
  Deriving both Clone and Copy uses the same generated Clone method.
