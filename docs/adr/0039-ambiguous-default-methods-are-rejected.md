# Ambiguous default methods are rejected

An inherent method on the receiver type takes precedence over default methods from traits the receiver explicitly implements. When no inherent method exists, method lookup may use a default only when exactly one applicable default-method declaration remains. Multiple applicable defaults with the same method name are a compile-time ambiguity rather than an iteration-order choice.

The diagnostic names every candidate trait. ADR-0102 later permits a caller to select one conformance through trait qualification; an inherent method remains the local way to give unqualified calls one meaning.

## Considered options

- Selecting the first applicable trait is simple, but makes behavior depend on symbol-table or import order.
- Selecting the most recently declared conformance makes declaration order alter method behavior.
- Trait-qualified calls resolve individual sites, but add syntax and still leave ordinary calls ambiguous.
- Requiring an inherent method gives the concrete type one explicit, local meaning for the method name.

## Consequences

- Existing inherent methods continue to win over defaults.
- Default candidates reached more than once through supertraits are deduplicated by their declaring method; distinct declarations remain ambiguous even when their stack effects match.
- The compiler reports all conflicting trait names instead of silently selecting one.
- Authors resolve a collision at a call site through trait qualification, for all unqualified calls with an inherent method, or by removing an unwanted conformance.
- Lookup already examines applicable trait defaults, so collecting candidates rather than returning the first adds negligible compile-time work.
