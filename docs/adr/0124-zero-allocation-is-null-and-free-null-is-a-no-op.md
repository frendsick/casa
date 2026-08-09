# Zero allocation is null and freeing null is a no-op

Raw allocation has one canonical empty representation:

```casa
0 alloc           # ptr::null
ptr::null free    # no-op
```

Allocating a positive byte count returns a non-null allocation or terminates immediately. The null result of `0 alloc` owns no storage and may be passed to `free` any number of times because `free` performs no operation for null.

## Consequences

- Empty containers may store `ptr::null` and use one unconditional cleanup path.
- `ptr::as_ref[T]`, `ptr::as_mut[T]`, `ptr::read[T]`, raw loads, raw stores, and pointer arithmetic still reject or have undefined behavior on null whenever they require accessible storage.
- The no-op rule applies only to null. Double-freeing any non-null allocation remains undefined behavior.
- A positive allocation never uses null as an out-of-memory result.
