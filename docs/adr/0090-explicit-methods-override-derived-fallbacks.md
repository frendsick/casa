# Explicit methods override derived fallbacks

Derivation supplies fallback methods and conformance. A handwritten method with the same trait hook and exact stack effect takes precedence, and the compiler generates only the missing methods:

```casa
struct Point derives Eq {
    x: i64
    y: i64
}

impl Point: PartialEq {
    fn eq $self other:$self -> bool {
        # custom equality
    }
}
```

The derived and handwritten declarations merge into one conformance for coherence purposes. This merge is allowed only between a type's own derive clause and one explicit customization block for the same conformance; two handwritten conformances remain a duplicate error. Collection and generation are source-order independent.

An explicit method with the wrong stack effect is diagnosed rather than ignored in favor of the fallback. Safe-code ownership and borrowing checks still apply to customized bodies; semantic trait laws remain the implementation author's responsibility unless a separate derivation-consistency rule says otherwise.

## Consequences

- `derives Eq`, `Ord`, `Hashable`, or `Clone` may be combined with custom hooks without reimplementing every generated method.
- Defaults and other generated methods dispatch to the winning explicit hook.
- `derives Copy` remains methodless; a custom Clone implementation overrides its fieldwise Clone fallback under ADR-0088.
- Derivation must run after collecting relevant explicit implementations so output does not depend on declaration order.
