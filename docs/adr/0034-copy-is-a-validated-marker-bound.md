# Copy is a validated marker bound
status: amended by [ADR-0158](0158-copy-requires-a-raw-value-representation.md)

Generic declarations express representation-safe duplication with the ordinary bound syntax `[T: Copy]`. Copy is a methodless standard-library marker trait with compiler-validated implementations. Built-in implementation is automatic, and user-defined structs and enums request it with `derives Copy` or an equivalent validated empty implementation. The intrinsic stack effects of `dup` and `over` require the canonical Copy trait on the value they duplicate.

A declared generic function whose body duplicates `T` or passes it to another `Copy`-bounded operation must expose `[T: Copy]`. Missing bounds are diagnosed at the generic definition rather than deferred until an incompatible instantiation. A function or lambda whose complete stack effect is inferred may infer the bound and includes it when displaying that inferred type.

## Considered options

- Inferring hidden bounds in every declared generic body keeps declarations short, but makes public contracts depend on implementation details and complicates higher-order checking.
- Leaving generic duplication unconstrained until monomorphization permits surprising call-site failures and prevents generic bodies from being validated independently.
- Trusting an unchecked marker implementation would let a type claim Copy without proving that raw duplication preserves ownership.
- A visible standard marker using existing bound syntax keeps the contract explicit while compiler validation preserves safety.

## Consequences

- `fn twice[T: Copy] value:T -> T T { value dup }` is valid; the same declaration without the bound is rejected.
- Bounds propagate through generic calls: a function accepting arbitrary `T` cannot call a `Copy`-bounded function without declaring or otherwise resolving that requirement.
- Concrete eligibility remains recursive and compiler-checked according to the affine ownership model; an eligible user-defined type remains move-only unless it implements Copy.
- Copy has a visible methodless trait declaration and uses ordinary explicit implementation syntax, while the compiler validates the representation-specific safety invariant.
- Bound checking is local and requires no speculative monomorphization, preserving predictable compile time.
