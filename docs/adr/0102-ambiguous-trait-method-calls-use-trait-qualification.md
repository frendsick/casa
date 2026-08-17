# Ambiguous trait method calls use trait qualification

When a receiver has multiple applicable trait implementation methods with the same name, a call selects the fully instantiated trait explicitly:

```casa
token Convert[i64]::convert
token Convert[str]::convert
```

The receiver value determines its concrete type and the qualifier selects its trait implementation. The ordinary postfix form remains available when method lookup finds one applicable implementation:

```casa
token.convert
```

## Consequences

- Method selection never depends on an expected return type.
- Adding another trait implementation can make an unqualified call ambiguous, but cannot silently redirect it.
- Ambiguity diagnostics list the qualifying trait names that make the call explicit.
- Trait qualification is required only at the ambiguous call site; implementations and unambiguous callers gain no extra syntax.
- Selecting a trait implementation method as a first-class function value requires a separate receiver-type decision because no receiver value is present.
