# Copy requires a raw value representation
related issue: #478

`Copy` is accepted only when duplicating the complete runtime value cannot
create aliases or require allocation. The compiler checks the value
representation before it checks fields and trait bounds.

Casa currently represents structs, arrays, and enums with payloads through an
owned heap pointer. These types cannot implement `Copy`, even when their fields
are Copy. Duplicating the pointer would create two apparent owners of one
allocation. Payload-free enums use a raw tag and remain eligible.

Explicit `Clone` remains available for independent aggregate duplication. A
future inline aggregate representation can restore Copy eligibility through a
successor ADR without weakening the allocation-free Copy contract.

## Consequences

- `derives Copy` and explicit Copy implementations use the same representation
  check.
- Structs with only scalar fields and empty structs are currently non-Copy.
- Enums with no payload can be Copy. Enums with any payload are non-Copy.
- Compiler-internal aggregate values use explicit Clone when they need an
  independent owner.
