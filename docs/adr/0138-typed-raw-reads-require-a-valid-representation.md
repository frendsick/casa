# Typed raw reads require a valid representation

`ptr::read[T]` performs no runtime representation validation. Its unsafe caller must guarantee that the initialized storage contains one valid `T`; reading invalid bits as `T` is undefined behavior:

```casa
2 address store8
address ptr::read[bool] # undefined behavior
```

Raw external or untrusted data is first loaded through `load8`, `load16`, `load32`, or `load64`, validated as the resulting unsigned integer, and then converted through a safe checked constructor.

## Consequences

- A valid `bool` representation contains only `0` or `1`.
- A valid `char` representation denotes a Unicode scalar and excludes surrogate values.
- An enum has a declared variant tag and a valid active payload.
- A borrow is non-null, live, correctly aligned, and satisfies its aliasing capability; an owner additionally satisfies uniqueness and its type invariants.
- Integer raw loads remain safe representations of arbitrary bits because every bit pattern is valid for the corresponding unsigned integer.
- `ptr::read[T]` stays a direct ownership move with no hidden branches; checked decoding belongs at the trust boundary.
