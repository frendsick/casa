# Lossy numeric conversions are explicit

Casa separates exact, checked, deliberately rounded, and wrapping numeric conversions:

- `Target::from` exists only when every source value has an exact representation in `Target`.
- `Target::try_from` returns ordinary `Option[Target]` and succeeds only when the particular source value has an exact representation in `Target`.
- `Target::round_from` performs an explicitly lossy IEEE floating-point conversion, including integer-to-float and `f64`-to-`f32` conversion.
- `Target::wrapping_from` is limited to integer-to-integer conversion and deliberately keeps the destination-width low bits.

## Consequences

- `f64::from` accepts `f32`, while `f32::from` does not accept `f64`.
- `f64::round_from` may round a large integer; `f32::round_from` may round an `f64` and produces signed infinity when a finite source overflows the destination width.
- A float-to-integer `try_from` rejects NaN, infinity, fractional values, and out-of-range values.
- A narrowing float-to-float `try_from` rejects values that are not represented exactly at the destination width.
- Casa initially adds no saturating numeric conversion.
- Numeric values never change type implicitly.
- `Option` remains ordinary standard-library code; this decision does not give it compiler-defined behavior.
