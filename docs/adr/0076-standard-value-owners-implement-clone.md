# Standard value owners implement Clone

Casa's standard value containers implement explicit `Clone` when all owned contents can be cloned. `str` and `Bytes` implement `Clone` directly. `array[T]`, `List[T]`, `Option[T]`, `Result[T E]`, `Map[K V]`, and `Set[T]` provide conditional ordinary trait implementations using the required `Clone` bounds on their owned type parameters.

These are library implementations, not compiler cases. In particular, `Option` and `Result` obey the same enum and trait rules as user-defined types. Implementations for uniquely owned mutable buffers produce independent backing storage. They may use bulk copying where element types are `Copy`, but the source operation remains explicit `clone`.

Owned closures, exclusive borrows, files, sockets, and other identity-bearing resource owners receive no automatic Clone implementation. A resource type may provide an explicitly named domain operation when duplicating its underlying capability is meaningful.

## Consequences

- Cloning nested standard containers works by composing their ordinary Clone implementations.
- A standard library may implement Clone for shared borrows directly; exclusive borrows do not implement it.
- Standard Clone implementations may allocate, and allocation failure terminates under ADR-0075.
- Adding a new owning standard type requires an explicit decision about whether it represents clonable value data or unique identity.
