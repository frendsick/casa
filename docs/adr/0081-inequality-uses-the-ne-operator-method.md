# Inequality uses the ne operator method

The `!=` operator lowers to the `ne` method of the active equality trait rather than always lowering to `eq` followed by boolean negation. The trait provides the ordinary default:

```casa
fn ne $self other:$self -> bool {
    other self.eq !
}
```

A type that implements the trait may override `ne` when it has a more direct implementation, but the trait contract requires `ne` to remain the logical inverse of `eq`. The compiler checks the operator method's stack effect but cannot prove that semantic law, just as it cannot prove reflexivity or transitivity.

## Consequences

- Current `OpValue::Ne` to `ne` method lowering is retained.
- A reserved equality trait must expose a correctly typed `ne` operator method, either with a body or as a required method.
- The standard declaration supplies the negating default so ordinary implementations define only their equality primitive.
- ADR-0082 places both operator methods on PartialEq and makes Eq inherit their effective shape.
