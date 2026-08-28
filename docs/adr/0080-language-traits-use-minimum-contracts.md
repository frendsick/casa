# Language-integrated traits use minimum contracts

Primitive operations have intrinsic semantics and remain available when no standard library is present. Traits connect the same syntax to user-defined types and generic bounds, but the compiler validates only the minimum method contract required by that language feature.

A declaration with a canonical standard-library language-trait identity must
provide each required method with the expected stack effect. An unrelated trait
with the same unqualified name is ordinary. A language trait may add default
methods and supertraits. Those remain ordinary trait behavior. Additional
bodyless required methods are rejected because compiler-provided primitive
implementations and derivation could not implement unknown behavior.

Copy has the smallest contract: it is a methodless marker whose implementations the compiler validates for representation-safe, allocation-free duplication. Its declaration may have ordinary supertraits, but the compiler does not require Clone unconditionally. Clone is guaranteed when the active Copy declaration extends Clone; Casa's standard declaration does, and a validated Copy implementation supplies missing fieldwise Clone behavior.

## Consequences

- Primitive arithmetic, comparison, and stack copying do not depend on importing trait declarations.
- Generic comparison and overloaded comparison for user types require active equality or ordering declarations with the complete effective operator-method stack effects.
- Display-backed formatting requires its declared formatting method for user-defined and generic values; primitive formatting must have an intrinsic freestanding path.
- `trait Copy { }` and `trait Copy: Clone { }` are both valid contracts in the
  canonical standard namespace. The latter imposes Clone through ordinary
  supertrait checking.
- A canonical Eq declaration such as `trait Eq { fn unrelated -> str }` is
  invalid because the equality operator method is missing.
- Current primitive comparison already bypasses trait dispatch; primitive printing and formatted strings require implementation work to gain the same freestanding behavior.
