# x86-64 Casa uses fixed-width size types

Casa's only compilation target is x86-64. It therefore uses `u64` for in-memory byte counts, collection lengths, capacities, indexes, allocation sizes, and shift counts. Genuinely signed offsets use `i64`.

Casa does not provide `usize` or `isize`. On the only target they would duplicate `u64` and `i64` while forcing explicit conversions between values with identical representation and range.

```casa
fn alloc bytes:u64 -> ptr
fn List::length $self -> u64
fn List::get $self index:u64 -> Option[$T]
```

## Consequences

- C `size_t` maps to `u64` and C `ssize_t` maps to `i64` in x86-64 foreign declarations.
- Fixed-width file, protocol, and persistent-format values already use the same explicit integer names but still require domain validation before allocation.
- Contextual integer literals adopt `u64` in size and index positions without suffix syntax.
- Casa adds no pointer-width integer aliases solely for hypothetical targets.
- A future target with a different pointer width must make a new layout and API decision rather than silently changing existing fixed-width source types.
