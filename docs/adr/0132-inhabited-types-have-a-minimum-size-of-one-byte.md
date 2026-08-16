# Inhabited types have a minimum size of one byte

Every inhabited concrete Casa type has `size_of[T] >= 1`. An empty struct therefore occupies one byte:

```casa
struct Marker { }

size_of[Marker] # 1
```

Casa does not implement zero-sized runtime values. The one-byte representation carries no user-visible field data but gives each stored element a nonzero stride and addressable location.

## Consequences

- Generic allocation uses `capacity size_of[T] *` uniformly for every inhabited `T`.
- Consecutive empty-struct elements have distinct addresses.
- `ptr::read[T]`, `ptr::write[T]`, iteration, and destruction need no null-storage exception for a present value.
- Empty structs remain Copy-eligible because their one-byte representation owns no resources.
- The small storage cost for collections of empty markers is accepted instead of adding zero-sized-type branches throughout compiler and library code.
