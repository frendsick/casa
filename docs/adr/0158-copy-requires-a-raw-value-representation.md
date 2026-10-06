# Copy requires a raw value representation
status: amended by [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md)
related issue: #478

`Copy` is accepted only when duplicating the complete runtime value cannot
duplicate ownership or exclusive access, or require allocation. The compiler
checks the value representation before it checks fields and trait bounds.

The compiler integrates the canonical standard-library methodless Copy marker
with implicit reuse, `dup`, and `over`. An unrelated trait named Copy does not
gain that behavior. A freestanding `std` may provide the canonical declaration
under [ADR-0080](0080-language-traits-use-minimum-contracts.md). Its bounds and
supertraits use ordinary trait machinery.

Casa represents ordinary structs and enums with payloads through an owned heap
pointer. These types cannot implement `Copy`, even when their fields are Copy.
Duplicating the pointer would create two apparent owners of one allocation.
Payload-free enums use a raw tag and remain eligible. Fixed arrays store their
elements directly and implement `Copy` when their element type implements
`Copy`.

Extern structs have a fixed C-layout body. An extern struct can implement
`Copy` when all owned fields implement `Copy` and stored borrows are shared.
Shared-borrow fields preserve their origins under ADR-0150. The compiler copies
the body into automatic destination storage instead of duplicating its temporary carrier
pointer. Escaping values receive ordinary owned storage as part of destination
placement.

Explicit `Clone` remains available for independent aggregate duplication. A
future direct representation for structs and payload enums can restore their
Copy eligibility through a successor ADR. Conditional array `Copy` does not
weaken the allocation-free Copy contract.

## Consequences

- User-defined structs and enums request Copy with `derives Copy` under
  [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md).
  Validation rejects exclusive borrows, owned indirection, and custom destruction.
  Stored shared borrows preserve their origins without borrow Copy conformance
  under [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md).
- Ordinary structs with only scalar fields and empty structs are non-Copy.
- Extern structs are Copy-eligible when every owned field is Copy and every stored borrow is shared.
- Enums with no payload can be Copy. Enums with any payload are non-Copy.
- Fixed arrays are conditionally Copy, including zero-length arrays.
- Compiler-internal aggregate values use explicit Clone when they need an
  independent owner.
