# Copy requires a raw value representation
related issue: #478

`Copy` is accepted only when duplicating the complete runtime value cannot
create aliases or require allocation. The compiler checks the value
representation before it checks fields and trait bounds.

Casa represents ordinary structs and enums with payloads through an owned heap
pointer. These types cannot implement `Copy`, even when their fields are Copy.
Duplicating the pointer would create two apparent owners of one allocation.
Payload-free enums use a raw tag and remain eligible. Fixed arrays store their
elements directly and implement `Copy` when their element type implements
`Copy`.

Extern structs have a fixed C-layout body. An extern struct can implement
`Copy` when all fields implement `Copy`. The compiler copies the body into
automatic destination storage instead of duplicating its temporary carrier
pointer. Escaping values receive ordinary owned storage as part of destination
placement.

Explicit `Clone` remains available for independent aggregate duplication. A
future direct representation for structs and payload enums can restore their
Copy eligibility through a successor ADR. Conditional array `Copy` does not
weaken the allocation-free Copy contract.

## Consequences

- `derives Copy` and explicit Copy implementations use the same representation
  check.
- Ordinary structs with only scalar fields and empty structs are non-Copy.
- Extern structs are Copy-eligible when every field is Copy.
- Enums with no payload can be Copy. Enums with any payload are non-Copy.
- Fixed arrays are conditionally Copy, including zero-length arrays.
- Compiler-internal aggregate values use explicit Clone when they need an
  independent owner.
