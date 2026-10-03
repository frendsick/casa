# Ordinary and extern structs share field storage planning
related issue: #610

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
stride are target-specific. Ownership, recursion cut points, indirect standalone
values, and the difference between physical compatibility and ABI stability are
language decisions.
