# Explicit unsafe boundary for unchecked operations

Casa code is safe by default. Raw allocation, pointer conversion and access, syscalls, unchecked collection access, and foreign calls require an explicit `unsafe` block. An `unsafe fn` may encapsulate these operations, but calling it also requires an unsafe context; a safe wrapper must establish and preserve its own invariants.

Checked collection access borrows elements. List access terminates on an invalid index, while Map access returns `Option`. List and Map removal return owned `Option[T]` values. Unchecked borrowed access remains available only inside `unsafe`. Recoverable failures use `Result` or `Option`. Violated program invariants use a small terminating `panic` path rather than exceptions.

## Considered options

- Leaving raw operations available everywhere would let safe-looking code bypass ownership and bounds guarantees.
- Removing low-level operations would prevent the self-hosted runtime, OS wrappers, and foreign interfaces Casa needs.
- Treating every failure as recoverable would make programmer invariants interrupt ordinary composition with unnecessary result handling.

## Consequences

- Safe Casa code cannot cause memory unsafety through compiler-provided operations.
- `List.get` returns `$T` and `List.get_mut` returns `mut$T`, with bounds checks. Map access returns `Option[$V]` or `Option[mut$V]`.
- Stdlib wrappers concentrate unsafe code and expose checked safe APIs.
- Constructing `$cstr` from a raw pointer requires `unsafe`; safe foreign-string wrappers preserve a source borrow or return an owned validated `String`.
