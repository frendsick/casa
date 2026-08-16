# Generics are monomorphized after one body check

A generic function body is typechecked once against its declared bounds. Reachable concrete type combinations are then monomorphized for ownership-aware lowering and code generation, giving each specialization direct layout, copy, destruction, and trait-method operations.

Casa initially passes no hidden type descriptors, destructor dictionaries, or trait-method dictionaries. It also avoids a hybrid erased/specialized strategy until measurement demonstrates that duplicated code generation is the dominant cost.

Monomorphization is expected to increase compilation time and binary size relative to today's mostly uniform-word generic compilation, in proportion to unique reachable instantiations. Before this decision is implemented permanently, benchmark compiler self-compilation time, produced compiler size, and generic-heavy programs. If the measured slowdown is material, return with the measurements and assumptions before retaining the strategy.
