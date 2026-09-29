# `size_of[T]` exposes inline storage size

status: amended by [ADR-0171](0171-constants-use-bounded-target-independent-expressions.md)

Casa provides the safe compile-time intrinsic:

```casa
size_of[T] # None -> u64
```

It returns the number of bytes occupied by one inline `T`, including tail padding required to keep consecutive values correctly aligned. The result is a compile-time constant after generic specialization.

ADR-0171 excludes this query from named constant initializers and constant type
arguments. Under ADR-0167, ordinary code retains a typed symbolic query until
target planning supplies its value. This separates target-neutral checking
from physical layout.

Generic raw-storage implementations use it to allocate and address dense elements:

```casa
capacity size_of[T] * alloc = data
index size_of[T] * data + = element_address
```

## Consequences

- `List[T]`, `array[T N]`, and user-defined unsafe containers can store multiword Copy aggregates inline without compiler-known collection types or one allocation per element.
- `ptr::read[T]` and `ptr::write[T]` use the same compiler layout when moving values through calculated addresses.
- Checked multiplication detects capacity-byte overflow before allocation.
- `size_of[T]` does not promise a stable foreign or persistent ABI. Layout may change between compiler versions unless a separate ABI feature says otherwise.
- `size_of[T]` is the only initial layout query. Casa exposes no `align_of[T]`, field-offset query, or packed-layout control. `alloc` provides sufficient base alignment and `size_of[T]` is a valid aligned array stride.
- Unsafe code obtains a field's actual address through typed field access followed by `ptr::from_ref`, rather than reconstructing its offset. Ordinary field access uses compiler-generated offsets.
- A future alignment, offset, or explicit-layout feature requires a concrete FFI, arena, or hardware-layout need and its own stability contract.
- Ordinary owned code does not need `size_of`; it is primarily a low-level implementation tool.
