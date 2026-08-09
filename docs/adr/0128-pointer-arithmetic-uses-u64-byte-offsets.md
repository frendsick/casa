# Pointer arithmetic uses `u64` byte offsets

Raw pointer arithmetic uses unsigned byte offsets:

```casa
ptr u64 + -> ptr
ptr u64 - -> ptr
```

Both operators require an unsafe context. The caller must ensure the result remains within the same live allocation or exactly one byte past its end. A one-past pointer may be stored and compared but not dereferenced, converted into a borrow, read, or written.

## Consequences

- Subtraction by `u64` provides backward movement without a signed offset type.
- Checked integer arithmetic used to calculate an offset occurs before the unsafe pointer operation.
- Pointer arithmetic on null, arithmetic overflow, or producing any other out-of-allocation address is undefined behavior.
- Casa initially provides no pointer-pointer subtraction or address-distance operation; containers retain indexes and lengths explicitly.
- Pointer arithmetic remains byte-based. Generic element addressing multiplies an index by `size_of[T]` first.
