# Floating-point types use explicit-width names

Casa names its IEEE-754 floating-point types `f32` and `f64`. The earlier `float` name is removed and does not remain as an alias.

## Consequences

- Source types, foreign declarations, layout queries, diagnostics, and documentation use the same `f32` and `f64` names.
- Both types support their corresponding IEEE-754 finite values, infinities, and NaNs.
- Both types implement PartialEq and PartialOrd, but not Eq, Ord, or Hashable.
- Integer-to-float, float-to-integer, and cross-width float conversions remain explicit.
- Casa adds no compatibility alias while it has no external compatibility requirement.
- Floating-point literals are context-typed under ADR-0141.
