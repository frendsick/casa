# Trait inheritance is acyclic

The supertrait graph must be a directed acyclic graph. The compiler rejects direct and indirect cycles:

```casa
trait A: B { }
trait B: A { } # error: A -> B -> A
```

Recursive data and recursive derivation obligations remain valid because they describe finite runtime structure or mutually dependent trait implementation checks. A supertrait cycle instead adds no capability while making required-method closure, default precedence, and trait satisfaction circular.

## Consequences

- Cycle detection runs after resolving trait names and before collecting inherited methods or checking trait implementations.
- Diagnostics report the complete shortest discovered cycle with declaration locations.
- Self-inheritance is rejected as the one-node case.
- Diamond inheritance remains possible because it is acyclic and receives separate duplicate-path handling.
