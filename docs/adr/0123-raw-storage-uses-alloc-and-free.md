# Raw storage uses `alloc` and `free`

Unsafe low-level code uses the minimal raw-storage pair:

```casa
u64 alloc -> ptr
ptr free -> None
```

`alloc` returns uninitialized storage aligned for every ordinary Casa type and terminates immediately if allocation fails. `free` releases one complete live allocation previously returned by `alloc` after every contained value has been moved out or destroyed.

ADR-0124 defines `0 alloc` as `ptr::null` and `ptr::null free` as a no-op. Every positive allocation remains non-null-or-terminate.

Both operations require an explicit unsafe block. Ordinary code receives owned values whose destruction performs any necessary reclamation internally.

## Consequences

- `free` releases raw storage only. It cannot discover or run destructors because `ptr` has no element type, initialized-length metadata, or ownership.
- Typed `drop` and raw `free` remain distinct: an owner destroys its value and fields before its implementation releases backing storage.
- Freeing an interior pointer, foreign pointer, already-freed allocation, or allocation containing live values is undefined behavior.
- Casa initially adds no `realloc`, `calloc`, allocator handles, arena selection, or explicit alignment argument.
- Containers grow by allocating new storage, moving initialized elements with `ptr::read[T]` and `ptr::write[T]`, then freeing the empty old allocation.
