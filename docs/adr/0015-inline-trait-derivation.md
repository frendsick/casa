# Inline derivation for selected capabilities
status: amended by [ADR-0158](0158-copy-requires-a-raw-value-representation.md)

Casa structs and enums request compiler-generated trait methods with an inline `derives` clause after the type name and any type parameters.

```casa
struct Point derives Eq Ord Hashable Copy {
    x: i64
    y: i64
}
```

Derivation is limited to `Eq`, `Ord`, `Hashable`, `Clone`, and `Copy`. Each implements the trait and generates any required methods. Copy remains methodless but requires additional compiler validation because it controls implicit duplication. Casa does not add a general attribute or metaprogramming system, and it does not derive `Display` because formatting is a design choice.

## Considered options

- A prefix directive keeps the existing type header unchanged, but weakens locality by separating generated behavior from the declaration it modifies.
- A separate derive declaration permits distant or cross-file derivation and therefore needs ordering, duplication, and trait implementation rules.
- General attribute syntax introduces an extensible metadata system to support one narrow compiler feature.
- An inline `derives` clause keeps the generated contract local and adds only one dedicated keyword.

## Consequences

- `derives` becomes a language keyword accepted on struct and enum declarations.
- Every derivation declares the same explicit trait implementation as an `impl Type: Trait` block while generating any required methods. `derives Copy` is equivalent to a validated empty Copy implementation and supplies missing fieldwise behavior required by the standard Copy trait's Clone supertrait.
- Implicit tag-only enum comparison and implicit enum hashing are removed. Enums opt in with `derives`, and generated methods include payload values. `derives Eq` generates PartialEq and Eq; `derives Ord` generates the total comparison primitives and implements PartialEq, Eq, PartialOrd, and Ord, with standard defaults supplying adapters and boolean operator methods.
- Struct equality and ordering visit fields lexicographically in declaration order. Enum ordering compares variant declaration order and then payloads. Hashing includes the variant tag and every equality-relevant field, with no cross-release stability guarantee.
- Generic derivation is conditional: `Pair[T] derives Eq Clone Copy` satisfies each capability only when the concrete `T` satisfies its corresponding requirement. Constructing another `Pair[T]` remains valid; a constrained use reports the unsatisfied bound.
- A handwritten trait method overrides the corresponding derived fallback under ADR-0090; only missing methods are generated. Two handwritten implementations remain a conflict.
- `derives Copy` is accepted only when every field is `Copy` and the type has no custom destruction. It generates no Copy method or copying code and must satisfy any supertraits declared by the active Copy contract; for standard Copy it supplies a missing fieldwise Clone implementation. `derives Clone` independently generates an ordinary explicit clone under ADR-0077 and does not imply `Copy`.
- Customizing `eq` disables independent field derivation of `hash` and `cmp`; ADR-0091 requires those requested companions to be implemented explicitly.
- Handwritten implementations remain available when generated behavior is unsuitable by omitting that trait from `derives` and declaring it on `impl Type: Trait`.
