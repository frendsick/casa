# One inherited default satisfies compatible requirements

One inherited default method satisfies every compatible bodyless requirement with the same name and stack effect:

```casa
trait Named {
    fn name $self -> str
}

trait Anonymous {
    fn name $self -> str { "unknown" }
}

trait Entity: Named + Anonymous { }
```

`Anonymous.name` is the single effective implementation of both inherited contracts. Explicitly inheriting both traits commits the child trait and the types that implement it to those contracts; the compiler cannot validate additional semantic laws.

## Consequences

- When compatible declarations have no body, an implementing type must provide one implementation.
- When exactly one compatible declaration has a body, that body supplies all of them.
- Multiple distinct inherited bodies remain ambiguous until the child trait or implementing type provides one overriding implementation.
- The rule applies after shared declarations from diamond inheritance are deduplicated.
