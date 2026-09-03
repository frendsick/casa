# Custom equality requires derived companions
status: superseded by [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md)

When a type customizes `eq`, the compiler cannot independently derive field-based behavior whose correctness depends on the equality relation. If the same type requests derived Hashable or Ord, it must also provide the corresponding `hash` or `cmp` method explicitly.

```casa
struct User derives Eq Hashable {
    id: i64
    name: str
}

impl User: PartialEq + Hashable {
    fn eq $self other:$self -> bool { ... }
    fn hash $self -> u64 { ... }
}
```

Generated companions remain available when they delegate to the customized method. The standard `ne` default calls `eq`; ordering boolean defaults call `partial_cmp`; Ord's partial adapter calls `cmp`. These defaults automatically observe customized behavior and require no duplicate implementation.

## Consequences

- Custom `eq` plus `derives Hashable` without explicit `hash` is a compile-time error.
- Custom `eq` plus `derives Ord` without explicit `cmp` is a compile-time error.
- The compiler validates presence and stack effects, not semantic consistency; explicit companion implementations remain the author's responsibility.
- This rule prevents silently generated field behavior from contradicting an explicit equality definition.
