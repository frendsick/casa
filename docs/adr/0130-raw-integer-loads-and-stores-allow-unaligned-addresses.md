# Raw integer loads and stores allow unaligned addresses

`load16`, `load32`, `load64`, and their matching stores permit any byte address on Casa's x86-64 target. The address need not satisfy the integer type's natural alignment:

```casa
buffer 1 + load32
```

The unsafe caller must still provide the complete number of accessible bytes. Crossing an allocation boundary or touching inaccessible memory is undefined behavior.

## Consequences

- Packed binary formats need no byte-by-byte reconstruction solely because a field is unaligned.
- The backend lowers these operations to ordinary x86-64 integer memory instructions without an alignment check.
- `load8` and `store8` are naturally unaffected.
- `ptr::read[T]`, `ptr::write[T]`, `ptr::as_ref[T]`, and `ptr::as_mut[T]` retain `T`'s natural-alignment requirement because they operate on typed storage.
- A future non-x86-64 target must lower the same language behavior correctly rather than silently imposing a new alignment precondition.
