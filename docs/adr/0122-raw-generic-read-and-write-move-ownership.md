# Raw generic read and write move ownership

Casa provides two unsafe generic intrinsics for implementing typed storage without representation casts:

```casa
value address ptr::write[T]
address ptr::read[T]
```

`ptr::write[T]` consumes and moves one valid `T` into correctly aligned writable storage that contains no live value. `ptr::read[T]` moves one valid initialized `T` out of correctly aligned readable storage and leaves that storage uninitialized.

These operations transfer ownership. They do not copy an owning representation or accept arbitrary bits as a valid `T`.

## Consequences

- Reading the same initialized slot twice is undefined behavior because the first read moves its owner out.
- Writing over a live value without first moving or destroying it is undefined behavior and may leak resources.
- Reading uninitialized storage, using the wrong `T`, violating alignment, or accessing invalid storage is undefined behavior.
- Unsafe collection implementations track which slots are initialized and ensure every live slot is moved or destroyed exactly once.
- `load8`, `load16`, `load32`, `load64`, and matching stores remain the raw numeric-bit operations; they do not replace typed ownership moves.
- The intrinsics lower directly and add no runtime type metadata or copy operation.
