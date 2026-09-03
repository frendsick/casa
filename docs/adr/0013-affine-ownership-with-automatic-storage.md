# Affine ownership with automatic storage placement
status: amended by [ADR-0027](0027-patterns-follow-subject-ownership.md), [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md), [ADR-0158](0158-copy-requires-a-raw-value-representation.md), and [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md)
related issue: #314

Casa will use affine ownership with compiler-checked borrowing and deterministic destruction. It will not use tracing garbage collection or automatic reference counting. Memory ownership is visible in function contracts, but allocator and arena selection are not part of ordinary source code.

Plain `T` parameters consume ownership. `$T` is a shared borrow and `mut$T` is an exclusive mutable borrow; the callee contract lets the compiler borrow automatically at call sites. Lifetimes are inferred with function-local analysis. A returned borrow is tied to every compatible borrowed input that could supply it; named lifetime parameters remain unsupported until a concrete need justifies them. ADR-0052 later permits borrowed values inside ordinary structs and enums under the same inferred origin analysis.

Lambda literals borrow captures by default. `move { ... }` transfers captured values into the closure when it must outlive their scope. `move` is a closure-construction modifier, not a general stack operation: assignments and consuming parameters already move owned values. Captured `Copy` values are copied. Every closure is repeatable; a closure that could consume a captured non-`Copy` value without restoring it before returning is rejected.

Higher-order function contracts reuse the same qualifiers instead of introducing separate callable type families. `$fn[...]` may call a closure repeatedly without changing its captures, `mut$fn[...]` may call it repeatedly while mutating captures, and plain `fn[...]` takes ownership of a repeatable callable. Call sites continue to borrow automatically from the declared parameter type.

## Considered options

- Tracing garbage collection hides ownership, but adds a runtime collector and nondeterministic reclamation.
- Automatic reference counting provides prompt reclamation for acyclic values, but adds hidden retain/release work and does not solve cycles without another mechanism.
- User-selected arenas provide control, but make allocation strategy part of routine program design before evidence shows that control is needed.
- Affine ownership with automatic storage placement makes resource lifetime explicit while keeping allocator choice out of normal code.

## Consequences

- Owned values move. `dup` and the copied operand of `over` require `Copy`; `swap` and `rot` only reorder ownership. Scalars, shared borrows, and named function references are automatically `Copy`; eligible user-defined aggregates opt in with `derives Copy` or a validated empty Copy implementation. Mutable borrows, heap owners, owned resources, and types with custom cleanup are not `Copy`.
- An escaping closure that borrows a local is rejected with a diagnostic suggesting `move { ... }`; captures are never silently moved or duplicated.
- `Slice[T]` is a runtime-length view tied to a borrowed list. The list cannot be mutated in a way that could invalidate the view before its last use.
- Collection observation preserves ownership: `get` returns `Option[$T]`, `get_mut` returns `Option[mut$T]`, and `remove` returns `Option[T]`. Generic wrappers such as `Option` may temporarily carry an inferred borrow without introducing named lifetime syntax.
- Collection iteration has three explicit modes: `iter` borrows and yields `$T`, `iter_mut` exclusively borrows and yields `mut$T`, and `into_iter` consumes and yields `T`. No mode implicitly duplicates elements.
- The stateful cursor trait is named `Iterator[T]`, not `Iterable[T]`; its required `next` method takes `mut$self` and returns `Option[T]`. A separate collection-to-iterator trait remains deferred.
- Field access copies `Copy` fields and borrows non-`Copy` fields. Partial moves are initially forbidden: an owned `match` consumes the complete subject, moves selected non-`Copy` fields, and destroys fields omitted by a partial pattern. A successful `is` pattern conditionally consumes an owned subject when it binds a non-`Copy` payload. Borrowed subjects bind payload borrows with the same capability.
- Observational trait methods borrow their receivers: comparison, hashing, and display take shared borrows. Operators and method calls borrow automatically, so observing an owned value does not consume it.
- Scope exit, early return, and explicit `drop` destroy each owner exactly once through the same lowering. A custom `fn drop mut$self` runs first, followed by fields in reverse declaration order; its mutable borrow prevents moving fields out during cleanup. A terminating panic does not unwind or run cleanup.
- The compiler may place non-escaping values on the stack or in internal scoped regions and uses a reclaimable heap when values escape or analysis is uncertain.
- Allocation is infallible at the Casa source level. A compiler-managed allocation either returns a valid owner or terminates the process immediately through an allocation-free fatal path; allocation does not return `Option` or `Result`, and Casa exposes no fallible allocation API.
- Storage placement does not change source semantics and may become more precise without source changes.
- Casa exposes no arena or allocator-selection API. That decision may be revisited if real workloads require explicit control.
- Cyclic or multiply referenced structures use one owner plus stable IDs, indices, or checked non-owning references. Raw pointers remain the explicit unsafe escape hatch.
