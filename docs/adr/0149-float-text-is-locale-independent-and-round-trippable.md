# Float text is locale-independent and round-trippable

Standard-library parsing accepts decimal floating-point text plus `NaN`, `inf`, and `-inf`, returning ordinary `Option[f32]` or `Option[f64]`. Formatting emits the shortest locale-independent decimal text that parses back to the same floating-point value at the same width.

## Consequences

- Formatting uses `.` as the decimal separator regardless of process locale.
- Negative zero formats as `-0.0` and therefore retains its sign through text round trips.
- Special values use the canonical spellings `NaN`, `inf`, and `-inf`.
- NaN payloads do not survive text conversion; exact representation transport uses `to_bits` and `from_bits`.
- Parsing is standard-library behavior, and `Option` receives no compiler special case.
