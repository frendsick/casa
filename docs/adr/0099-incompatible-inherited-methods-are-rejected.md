# Incompatible inherited methods are rejected

A trait may not inherit distinct methods with the same name and incompatible stack effects:

```casa
trait Numeric {
    fn value $self -> i64
}

trait Textual {
    fn value $self -> str
}

trait Both: Numeric + Textual { } # error
```

Casa has no method overloading, so no conformance could provide both meanings of `value`. The compiler rejects the composition at `Both` rather than leaving every attempted implementation to fail later.

## Consequences

- Compatibility includes the complete instantiated stack effect, receiver ownership, generic bounds, and method type parameters.
- Compatible bodyless declarations may be satisfied by one implementation.
- Shared declarations reached through a diamond are first deduplicated under ADR-0097 and therefore do not conflict with themselves.
- Diagnostics identify both inherited declarations and the paths through which they reached the child trait.
