# Multiple generic bounds use plus

A type variable may require multiple capabilities by separating bounds with `+`: `[T: Copy + Display]`. Every listed bound must be satisfied. The order is presentation only; semantically the bounds form a set.

Casa reuses the existing `+` spelling for multiple supertraits. It does not initially add `where` clauses, trait aliases, comma-separated alternatives, or disjunctive bounds.

```casa
fn show_twice[T: Copy + Display] value:T {
    value dup
    print
    print
}
```

## Considered options

- Allowing only one bound keeps the current representation, but cannot express code that both duplicates and observes a generic value.
- Nested helper traits could combine requirements, but would create otherwise meaningless declarations and extra conformance boilerplate.
- `where` clauses scale to large contracts, but add a second bounds grammar before Casa has contracts large enough to need it.
- Reusing `+` is compact and matches existing supertrait syntax.

## Consequences

- The compiler stores a list or set of bounds for each type variable rather than one `TraitBound`.
- Duplicate equivalent bounds are diagnosed or normalized; they do not create duplicate method requirements.
- If two bounds expose the same method name with incompatible stack effects, the generic declaration is rejected with both candidates in the diagnostic.
- Bound validation remains local to the generic declaration and grows only with the number of explicitly written bounds.
