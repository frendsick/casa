# Aggregate padding is unspecified and may be uninitialized

Padding inserted by the compiler is not part of a Casa value's semantic data. Its bytes have unspecified contents and may remain uninitialized:

```casa
struct P derives Copy {
    small: u8
    large: u64
}
```

Any bytes between `small` and `large`, or after the final field, exist only to satisfy the current layout. Safe code cannot observe them.

## Consequences

- Compiler-generated Copy may copy initialized fields or propagate opaque representation bytes without making padding observable as values.
- Equality, ordering, hashing, Clone generation, and serialization operate on fields and variants rather than comparing raw aggregate bytes.
- `size_of[T]` includes padding for allocation and stride but does not imply that every byte may be read as an initialized `u8`.
- The ordinary stdlib `memcpy` is valid only when the caller knows every copied source byte is initialized; a Copy implementation alone does not establish that for aggregate padding.
- Casa does not zero padding on construction merely to stabilize unused bytes or make raw representation serialization possible.
