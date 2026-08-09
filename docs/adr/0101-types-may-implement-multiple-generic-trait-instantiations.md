# Types may implement multiple generic trait instantiations

A receiver type may conform to more than one distinct instantiation of the same generic trait:

```casa
trait Convert[T] {
    fn convert $self -> T
}

impl Token: Convert[i64] {
    fn convert $self -> i64 { ... }
}

impl Token: Convert[str] {
    fn convert $self -> str { ... }
}
```

The coherence identity is the receiver type plus the fully instantiated trait, not merely the receiver and trait name. The two conformances above are distinct; a second `Token: Convert[i64]` remains a duplicate.

## Consequences

- Conformance methods retain their instantiated-trait identity instead of being flattened solely by receiver and method name.
- Generic conformance patterns are rejected only when they can overlap at the same fully instantiated trait.
- Bounds select the matching trait instantiation exactly.
- An unqualified method call is valid only when its conformance is otherwise unambiguous; explicit call-disambiguation syntax is a separate decision.
- Inheriting incompatible instantiations into one child trait remains subject to ADR-0099's method-conflict rule.
