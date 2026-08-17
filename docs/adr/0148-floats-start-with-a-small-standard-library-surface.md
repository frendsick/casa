# Floats start with a small standard-library surface

The initial standard library exposes these associated values and methods for both `f32` and `f64`:

- `nan`, `infinity`, `neg_infinity`, and `epsilon`
- `is_nan`, `is_infinite`, and `is_finite`
- `abs`, `floor`, `ceil`, `trunc`, and `round`

`round` selects the nearest integral floating-point value with ties-to-even. These names are ordinary standard-library APIs; the compiler does not recognize them as traits or language trait methods.

## Consequences

- Special values need no dedicated literal syntax and can be constructed from their standard IEEE representations.
- Basic classification and rounding do not require a general cast.
- Transcendental functions, `total_cmp`, additional constants, and a comprehensive math module are deferred until concrete programs need them.
- Narrow compiler primitives may support implementations where ordinary arithmetic and bit conversion are insufficient without making the public method names compiler-owned.
