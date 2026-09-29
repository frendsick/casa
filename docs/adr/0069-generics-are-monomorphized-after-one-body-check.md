# Generics are monomorphized after one body check

status: amended by [ADR-0170](0170-generics-specialize-after-symbolic-checking.md)

A generic function body is typechecked once against its declared bounds. Reachable concrete type combinations are then monomorphized for ownership-aware lowering and code generation, giving each specialization direct layout, copy, destruction, and trait-method operations.

Casa initially passes no hidden type descriptors, destructor dictionaries, or trait-method dictionaries. It also avoids a hybrid erased/specialized strategy until measurement demonstrates that duplicated code generation is the dominant cost.

Unique reachable instantiations can increase compilation time and emitted size. The [checked-generics benchmark](../benchmarks/checked-generics.md) records the measured tradeoff. Reconsider material slowdowns using self-compilation, produced compiler size, and generic-heavy workloads.
