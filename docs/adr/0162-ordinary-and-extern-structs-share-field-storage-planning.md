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

The representation change is staged. The first implementation centralizes the
storage plan and preserves existing placement where that placement is internally
consistent. It also fixes existing disagreements between layout and accessors.
After address-based projection and destruction are available, an eligible
concrete nested struct field in an ordinary struct uses its complete target
layout inline. This rule does not depend on whether the nested declaration is
ordinary or extern. An eligible field graph is nonempty, nongeneric,
nonrecursive, has no borrow or field that needs nontrivial destruction, and uses
the field forms accepted for an extern C layout. The following cases remain
indirect:

- an unresolved generic field
- an enum payload, including an edge that closes recursive ownership
- a nested struct that does not meet the eligibility rules
- any other edge whose complete layout is not known without recursion

Generic structs keep one declaration-time layout. Specialization-dependent
offsets are deferred. Borrowed fields remain pointer-sized fields. A standalone
ordinary struct value remains one owning pointer to its body. This keeps current
move, `Copy`, `ptr::from_raw`, and `ptr::into_raw` behavior.

Inlining must preserve field replacement, ownership transfer, borrow projection,
custom cleanup, and reverse-order field destruction. Projection and destruction
of an inline owner operate on its address. They do not materialize a temporary
heap owner. Moving an inline field out can allocate the owning shell required by
the unchanged standalone representation.

On the current x86-64 target, this design lets a nonempty ordinary body match C
when its flattened field graph uses supported scalar, pointer, fixed-array, and
concrete nested layouts. Empty structs, zero-length arrays, unresolved generic
fields, enums, and recursive cut points retain Casa-specific layouts. Equal body
layout does not make ownership, borrowing, destruction, the standalone value
carrier, or the native calling convention equivalent to C. By-value extern
parameters and returns remain separate work.

The shared plan is target-specific where C size, alignment, padding, and array
stride are target-specific. Ownership, recursion cut points, indirect standalone
values, and the difference between physical compatibility and ABI stability are
language decisions.

## Consequences

- The compiler has one source of truth for aggregate field placement and field
  operations.
- Extern validation and ABI stability remain explicit without a second layout
  engine.
- Eligible nested fields can remove persistent child allocations and can improve
  C body compatibility.
- Inlining can enlarge an outer body and changes `size_of`, member offsets, and
  unsafe raw-storage assumptions. ADR-0127 permits these ordinary-layout changes,
  but releases must document them and tests must state which representation layer
  they check.
- Inlining has no general performance guarantee. The investigation prototype
  reduced live body storage but was slower because destruction allocated a
  temporary owner. Default inlining waits until the affected paths add no such
  allocation.
- The accepted implementation is split into focused changes. It does not require
  a full struct value-representation rewrite.

The supporting trace, compatibility matrix, prototype results, and option
comparison are in
[the ordinary struct C-compatibility investigation](../benchmarks/ordinary-struct-c-compatibility.md).
Owned-field replacement is tracked by #619, shared storage planning by #620,
and concrete field inlining by #621. Issue #621 is blocked by the two correctness
prerequisites.
