# Subtrait implementation satisfies supertraits

Implementing a trait satisfies every transitive supertrait after all effective inherited requirements are met:

```casa
trait Eq: PartialEq { ... }

impl Point: Eq {
    fn eq $self other:$Point -> bool { ... }
    fn ne $self other:$Point -> bool { ... }
}
```

`Point` may be used wherever `PartialEq` is required without a separate implementation. Likewise, a generic `[T: Eq]` bound satisfies a `[T: PartialEq]` requirement.

## Consequences

- The subtrait block may satisfy inherited bodyless requirements or use inherited defaults.
- An existing explicit supertrait implementation is reused rather than duplicated.
- Listing both a trait and its supertrait in one implementation is accepted and normalized to one effective implementation per type-trait pair.
- Separate explicit supertrait and subtrait blocks may coexist when their method bodies do not conflict.
- Conflicting handwritten method bodies and duplicate direct implementations remain errors under the ordinary coherence rules.
- Implied trait satisfaction does not weaken the orphan rule: the direct subtrait implementation must still be declared by the type or trait owner.
