# Safe truncating conversion keeps Option in the library

Each integer target provides a compiler primitive `Target::trunc_from` for `f32` and `f64` sources. It truncates a fractional value toward zero and terminates when the source is NaN, infinite, or outside the target's range.

The standard library implements exact `Target::try_from` conversion by checking finiteness, integrality, and range before calling `trunc_from`, then returning its ordinary `Option[Target]` result.

## Consequences

- `trunc_from` is safe because every call either returns a valid target integer or terminates; it cannot construct an invalid value.
- Users may call `trunc_from` directly when truncation is intended and exceptional inputs should terminate.
- `try_from` remains non-terminating for conversion failure and returns `Option::None` instead.
- The compiler knows the numeric primitive but neither recognizes `Option` nor lowers the standard-library wrapper specially.
- Narrowing float-to-float `try_from` is likewise implemented in the library using `round_from`, exactness checks, and ordinary `Option` construction.
