# Copy requires a raw value representation
related issue: #478

`Copy` is accepted only when duplicating the complete runtime value cannot
create aliases or require allocation. The compiler checks the value
representation before it checks fields and trait bounds.

Casa currently represents structs and enums with payloads through an owned heap
pointer. These types cannot implement `Copy`, even when their fields are Copy.
Duplicating the pointer would create two apparent owners of one allocation.
Payload-free enums use a raw tag and remain eligible. Fixed arrays store their
elements directly, but the current array rule still excludes them from `Copy`.

Explicit `Clone` remains available for independent aggregate duplication. A
future direct representation for structs and payload enums can restore their
Copy eligibility through a successor ADR. Fixed arrays need a separate rule
change to become conditionally Copy. Neither change weakens the allocation-free
Copy contract.

## Consequences

- `derives Copy` and explicit Copy implementations use the same representation
  check.
- Structs with only scalar fields and empty structs are currently non-Copy.
- Enums with no payload can be Copy. Enums with any payload are non-Copy.
- Fixed arrays remain non-Copy until the array rule permits conditional Copy.
- Compiler-internal aggregate values use explicit Clone when they need an
  independent owner.
