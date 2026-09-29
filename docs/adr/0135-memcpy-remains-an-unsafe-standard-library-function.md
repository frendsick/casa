# `memcpy` remains an unsafe standard-library function

Casa keeps `memcpy` as an ordinary unsafe standard-library function. It remains
auditable Casa source using raw load, store, and pointer arithmetic.

The caller supplies `count` initialized readable source bytes and writable
destination bytes. The regions must not overlap.

## Consequences

- `memcpy` is appropriate for fully initialized byte storage, including `Bytes`, text backing buffers, and foreign byte arrays. A Copy implementation alone does not prove that aggregate padding is initialized.
- Byte-copying a non-Copy typed representation duplicates ownership and is undefined behavior; typed containers move elements with `ptr::read[T]` and `ptr::write[T]`.
- Casa initially adds no `memmove`; an actual overlapping-copy caller may justify the ordinary library function later.
- The compiler receives no special byte-copy operation initially. Benchmarks may justify an x86-64 backend intrinsic or optimized implementation if the loop is materially hot.
