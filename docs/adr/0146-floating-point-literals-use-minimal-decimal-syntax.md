# Floating-point literals use minimal decimal syntax

Casa floating-point literals use decimal notation with either a decimal point or an exponent:

```casa
1.0
1e3
6.02e23
1.5e-2
```

A decimal point requires digits on both sides. An exponent uses `e` or `E` followed by an optional sign and at least one digit.

## Consequences

- `.5` and `1.` are rejected; users write `0.5` and `1.0`.
- Casa initially adds no hexadecimal floating-point notation or type suffixes.
- NaN and infinities have no literal syntax; standard-library associated values construct them.
- Literal typing and direct target-width rounding follow ADR-0141.
