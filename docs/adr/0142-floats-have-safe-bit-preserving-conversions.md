# Floats have safe bit-preserving conversions

Casa provides named compiler primitives for exact IEEE representation conversion:

```casa
bits f32::from_bits # u32 -> f32
value.to_bits       # $f32 -> u32

bits f64::from_bits # u64 -> f64
value.to_bits       # $f64 -> u64
```

## Consequences

- Every input bit pattern is accepted because every `u32` or `u64` pattern denotes an IEEE floating-point value, including NaNs and infinities.
- These operations are safe, allocation-free, and preserve every bit when round-tripped without intervening arithmetic.
- They reinterpret representation rather than perform numeric conversion.
- Floating-point arithmetic may change a NaN payload; only the direct bit conversion contract preserves it.
- Casa needs neither a general cast nor unsafe pointer access for this operation.
