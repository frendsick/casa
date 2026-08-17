# Destruction is LIFO

Owned locals and temporaries are destroyed in reverse successful initialization order when their scope ends. A returned owner moves out before cleanup, reassignment destroys the replaced owner immediately, and fields are destroyed in reverse declaration order after any reserved `drop` method runs.

The same ordering applies on ordinary fallthrough and early `return`. A value that was moved is not destroyed unless it was subsequently reinitialized. Terminating `panic` and explicit process `exit` do not unwind or run cleanup.

LIFO follows Casa's value stack, makes custom cleanup order deterministic, and needs no dependency graph or user-facing cleanup-order syntax.

ADR-0095 initially applies the same order through call-stack-recursive destruction of recursive owned values. Any later iterative lowering must preserve these observable cleanup-method and field-order semantics.
