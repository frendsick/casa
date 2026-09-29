# Language-integrated traits use minimum contracts

Primitive operations have intrinsic semantics and remain available when no standard library is present. Traits connect the same syntax to user-defined types and generic bounds, but the compiler validates only the minimum method contract required by that language feature.

A declaration with a canonical standard-library language-trait identity must
provide each required method with the expected stack effect. An unrelated trait
with the same unqualified name is ordinary. A language trait may add default
methods and supertraits. Those remain ordinary trait behavior. Additional
bodyless required methods are rejected because compiler-provided primitive
implementations and derivation could not implement unknown behavior.

Copy has the smallest contract: it is a methodless marker whose implementations the compiler validates for representation-safe, allocation-free duplication. Its declaration may have ordinary supertraits, but the compiler does not require Clone unconditionally. Casa's standard declaration is `trait Copy: Clone { }`, so every standard Copy type also satisfies Clone through ordinary supertrait checking.

When that declaration is active, `derives Copy` supplies complete structural
Clone behavior under
[ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md).
An explicit Clone implementation cannot replace part of the derived family.
The compiler does not synthesize behavior for unrelated Copy supertraits.
A freestanding canonical `trait Copy { }` permits Copy without Clone, so its
Copy-only types cannot satisfy a Clone bound or Clone derivation until they
also implement Clone. Generic explicit duplication uses a Clone bound, while
implicit and stack duplication use Copy.

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
