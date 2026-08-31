# Copy never allocates

`Copy` duplicates a fixed inline representation without invoking user code or requesting dynamic allocation. Implicit binding reuse, `dup`, and the copied operand of `over` use only this operation. Copying may take time proportional to the representation size, but it cannot traverse owned indirection or manufacture another owned allocation.

Automatic storage placement remains separate. The compiler may allocate an enclosing or escaping destination just as it would when constructing the value there, but performing `Copy` introduces no additional backing allocation. Lowering writes directly into caller-provided destination storage and should avoid temporary buffers.

An `array[T N]` is `Copy` when `T` is `Copy`. Its fixed inline storage is copied
directly, including when `N` is zero. A Copy extern struct uses the same rule
for its fixed C-layout body. Types that own indirect storage, including
`String`, `Bytes`, dynamic collections, and compiler-indirected recursive
values, are never `Copy`. Allocation does not make raw duplication of their
owning handles safe. A `str` is a non-owning view, so copying it copies only its
fixed representation.

## Consequences

- `Copy` remains safe to invoke implicitly because it cannot hide allocator or user-defined work.
- Large inline aggregates may still make copying expensive; benchmarks must cover large copies in long loops.
- Arrays with non-`Copy` elements remain affine and use explicit `Clone` when
  their elements implement `Clone`.
- The implementation must verify with allocation instrumentation that copying an already placed value requests no allocation.
- Explicit duplication that may allocate belongs to `Clone` and is never selected by stack intrinsics or implicit reuse.
