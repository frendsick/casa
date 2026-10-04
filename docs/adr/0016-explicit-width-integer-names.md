# Explicit-width numeric names

Casa integer types use explicit-width names. The existing `int` type is renamed to `i64`, and the proposed `byte` name is replaced by `u8`. Casa provides `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, and `u64`. ADR-0125 removes `isize` and `usize` because x86-64 is Casa's only compilation target.

IEEE-754 floating-point types use `f32` and `f64`. The earlier `float` name is
removed and does not remain as an alias. Source types, foreign declarations,
layout queries, diagnostics, and documentation use the same explicit-width names.
Floating comparison follows [ADR-0012](0012-ieee-float-with-partial-comparison.md), literals
follow [ADR-0141](0141-floating-point-literals-are-contextual.md),
and conversions remain explicit under
[ADR-0143](0143-lossy-numeric-conversions-are-explicit.md).

## Considered options

- Keeping `int` and `byte` as canonical names is concise, but hides representation at binary and foreign-function boundaries.
- Keeping them as aliases for `i64` and `u8` preserves compatibility, but creates two names for the same type while source compatibility is intentionally breakable.
- Explicit-width names make layout and conversion costs visible without a separate family of C-only types.

## Consequences

- Existing `int` source migrates to `i64`. `int` does not remain as an alias.
- Binary data and UTF-8 code units use `u8`. `byte` is not a separate type or alias.
- In-memory sizes and indexes use `u64`. Signed offsets use `i64`.
- Numeric conversions are explicit. Casa does not add implicit integer promotion.
- Integer literals are typed from immediate context and default to `i64` only when otherwise unconstrained. This does not convert already typed values.
