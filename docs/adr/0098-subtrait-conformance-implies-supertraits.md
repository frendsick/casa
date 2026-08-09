# Subtrait conformance implies supertraits

Conformance to a trait establishes conformance to every transitive supertrait after all effective inherited requirements have been satisfied:

```casa
trait Eq: PartialEq { ... }

impl Point: Eq {
    fn eq $self other:$Point -> bool { ... }
    fn ne $self other:$Point -> bool { ... }
}
```

`Point` may be used wherever `PartialEq` is required without a separate conformance declaration. Likewise, a generic `[T: Eq]` bound satisfies a `[T: PartialEq]` requirement.

## Consequences

- The subtrait block may satisfy inherited bodyless requirements or use inherited defaults.
- An existing explicit supertrait conformance is reused rather than duplicated.
- Listing both a trait and its supertrait in one conformance is accepted and normalized to one effective conformance per type-trait pair.
- Separate explicit supertrait and subtrait blocks may coexist when their method bodies do not conflict.
- Conflicting handwritten method bodies and duplicate direct conformances remain errors under the ordinary coherence rules.
- Implied conformances do not weaken the orphan rule: the direct subtrait conformance must still be declared by the type or trait owner.
