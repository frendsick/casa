# Floating-point execution preserves strict IEEE semantics

Casa uses round-to-nearest, ties-to-even for ordinary floating-point execution. It preserves subnormal values and signed zero.

## Consequences

- The compiler does not reassociate floating-point expressions, silently fuse multiply-add, assume values are finite, discard signed zero, or flush subnormals to zero.
- NaN payload propagation through arithmetic is not specified; exact payload preservation remains limited to direct `from_bits`/`to_bits` round trips.
- Casa initially exposes no ambient rounding-mode control or floating-point exception flags.
- Fast-math modes and an explicit fused `mul_add` operation are deferred until measured programs need them.
- These rules constrain runtime optimization, not the accepted source syntax.
