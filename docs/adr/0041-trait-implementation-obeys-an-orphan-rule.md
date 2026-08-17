# Trait implementation obeys an orphan rule

An explicit `impl Type: Trait` implementation may be declared only in a module that defines the receiver type or the trait. A third module that owns neither side cannot connect two imported declarations.

Each concrete receiver type and fully instantiated trait pair has at most one implementation in a program. Distinct instantiations of one generic trait may coexist. Generic implementations whose receiver and trait-argument patterns may overlap at the same instantiation are rejected rather than ordered or specialized. A `derives Trait` clause counts as the implementation declared by the type's defining module.

## Considered options

- Allowing implementations anywhere maximizes extension, but lets unrelated imports change bound satisfaction and default-method lookup.
- Restricting implementations to the type's module gives one owner, but prevents a newly defined local trait from being implemented for built-in or imported types.
- Restricting implementations to the trait's module prevents a local type from adopting an imported trait.
- Allowing either owner preserves both common extension directions while excluding third-party combinations.

## Consequences

- A module defining `Json` may implement `Json` for an imported type; a module defining `User` may implement an imported trait for `User`.
- Duplicate and potentially overlapping implementations are compile-time errors with both declaration locations.
- Casa initially has no trait implementation specialization, priority ordering, negative implementations, or unrestricted `impl[T] T: Trait` blanket implementations.
- Trait implementation lookup is deterministic and can be indexed by fully instantiated trait and receiver type rather than searching for matching methods.
- Derived implementations participate in overlap checks. One explicit implementation in the type's module may customize that derived implementation under ADR-0090; it merges with the derive rather than counting as a second implementation. Two explicit implementations remain duplicates.
- Modules remain free to provide ordinary wrapper functions when the orphan rule prevents a trait implementation.
