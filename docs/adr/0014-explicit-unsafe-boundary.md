# Explicit unsafe boundary for unchecked operations

Casa code is safe by default. Raw allocation, pointer conversion and access, syscalls, unchecked collection access, and foreign calls require an explicit `unsafe` block. An `unsafe fn` may encapsulate these operations, but calling it also requires an unsafe context; a safe wrapper must establish and preserve its own invariants.

Ordinary collection access returns a borrow such as `Option[$T]`; removing an element returns owned `Option[T]`. Unchecked borrowed access remains available only inside `unsafe`. Recoverable failures use `Result` or `Option`; violated program invariants use a small terminating `panic` path rather than exceptions.

## Considered options

- Leaving raw operations available everywhere would let safe-looking code bypass ownership and bounds guarantees.
- Removing low-level operations would prevent the self-hosted runtime, OS wrappers, and foreign interfaces Casa needs.
- Treating every failure as recoverable would make programmer invariants interrupt ordinary composition with unnecessary result handling.
- A lexical unsafe boundary keeps low-level capability while making the trusted surface visible and auditable.

## Consequences

- Safe Casa code cannot cause memory unsafety through compiler-provided operations.
- `get` returns `Option[$T]`, `get_mut` returns `Option[mut$T]`, and their unchecked forms require `unsafe`.
- Stdlib wrappers concentrate unsafe code and expose checked safe APIs.
- Constructing `$cstr` from a raw pointer requires `unsafe`; safe foreign-string wrappers preserve a source borrow or return an owned validated `String`.
- Existing compiler and stdlib pointer/syscall code requires an explicit migration.
- Casa does not add exception handling.
