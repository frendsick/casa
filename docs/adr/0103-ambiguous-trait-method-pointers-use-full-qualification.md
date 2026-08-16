# Ambiguous trait method pointers use full qualification

Selecting a conformance method as a first-class function value names both the receiver type and fully instantiated trait when ordinary lookup would be ambiguous:

```casa
&Token::Convert[i64]::convert
&Token::Convert[str]::convert
```

There is no receiver value at the reference site from which to determine the concrete type. Full qualification therefore identifies one conformance and one method without expected-type inference.

## Consequences

- `&Token::convert` remains valid when method lookup is unambiguous.
- The qualified reference has the concrete method's ordinary function type; qualification does not create a trait object or runtime dispatch.
- Generic code may continue to use `&T::method` when its bounds select one applicable conformance.
- Function-reference resolution does not depend on a later assignment, argument, or return type.
- Wrapper lambdas are unnecessary solely to disambiguate a conformance method.
