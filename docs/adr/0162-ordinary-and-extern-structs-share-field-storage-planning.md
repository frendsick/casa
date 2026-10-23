# Ordinary and extern structs share field storage planning
related issues: #610, #839

Ordinary and extern structs use one target-specific field storage plan. The plan
defines each field's size, alignment, offset, inline or indirect placement, and
the operations needed to project, load, store, move, and destroy it. Layout,
construction, generated accessors, patterns, raw typed operations, and
destruction consume this plan instead of making separate placement decisions.

`extern struct` remains distinct syntax and remains the stable C ABI contract. It
validates that a field graph is supported by the target C ABI and permits the
validated type at supported native boundaries. It does not select a separate
field placement algorithm. A matching ordinary struct can have the same physical
body in one compiler build without gaining ABI stability or native-call
eligibility.

The shared plan is target-specific where C size, alignment, padding, and array
stride are target-specific. Ownership, recursion cut points, and the difference
between physical compatibility and ABI stability are language decisions.
Standalone storage placement is a backend decision.

## Standalone aggregate storage

Structs and payload enums with bodies of at most 4096 bytes use automatic
storage for standalone values, including affine owners. Consuming calls transfer
the body into the callee's storage.
Results use destination storage owned by the caller, including early returns,
multiple results, and function-value calls. Moving an affine body transfers its
fields without cloning them or calling its cleanup method. Custom destruction
and reverse-field cleanup still run once for each live owner.

This extends the destination convention from [ADR-0158](0158-copy-requires-a-raw-value-representation.md)
without granting Copy to affine types or changing their field layout. Recursive
edges retain the indirection required by [ADR-0017](0017-compiler-managed-recursive-indirection.md).
Indirect owned fields, owned closure captures, and raw-owner escapes receive
independent heap storage. Moving a body out of that storage releases only its
old backing allocation. Borrowed projections retain the source owner's storage.

Automatic bodies and result destinations occupy the function frame. The former
4096-byte budget per function is replaced by a 4096-byte limit per affine body.
Larger affine bodies retain heap storage across calls. Copy aggregates keep
their allocation-free representation at every size. Frame storage uses the
existing return-stack bound and overflow checks. Large bodies and repeated
move temporaries can increase frame size and compilation cost. Reuse destination
slots when measured workloads justify lifetime-based slot planning. Non-Copy
arrays keep their existing storage convention.
