# Trait defaults resolve methods in their own conformance

Inside a trait default method, an unqualified call to another method declared by that trait resolves within the same instantiated conformance:

```casa
trait Convert[T] {
    fn convert $self -> T

    fn optional $self -> Option[T] {
        self.convert Option::Some
    }
}
```

If `Token` implements both `Convert[i64]` and `Convert[str]`, the first `optional` specialization calls `Convert[i64]::convert` and the second calls `Convert[str]::convert`. No runtime dispatch or expected-return-type inference is involved.

## Consequences

- The enclosing instantiated trait is the lexical lookup context for its default bodies.
- Default methods remain source-generic and are specialized with their conformance under ordinary monomorphization.
- A default intentionally calling another trait uses explicit qualification, such as `self OtherTrait::method`.
- A trait's own requirement wins only through this lexical context; ordinary call sites retain the normal inherent-method and ambiguity rules.
