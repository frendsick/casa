# `size_of[T]` exposes inline storage size

Casa provides the safe compile-time intrinsic:

```casa
size_of[T] # None -> u64
```

It returns the number of bytes occupied by one inline `T`, including tail padding required to keep consecutive values correctly aligned. The result is a compile-time constant after generic specialization.

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
- Casa initially needs no `align_of[T]`: `alloc` provides sufficient base alignment and `size_of[T]` is a valid aligned array stride.
- Ordinary owned code does not need `size_of`; it is primarily a low-level implementation tool.
