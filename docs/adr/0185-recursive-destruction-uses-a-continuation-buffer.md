# Recursive destruction uses a continuation buffer

Compiler-generated destruction of recursive owned structs, enums, arrays, and closure captures uses bounded return-stack space at any depth. The compiler finds each cycle of destruction functions that can call each other, including branching, mutually recursive, inline-field, array, and closure record layouts. One worker destroys every member of a cycle in a single frame. Before it destroys a child in the same cycle, it saves the parent's continuation in a growable buffer and resumes it after the child is destroyed. Destruction of a type outside a cycle keeps the direct recursive path.

A function value can hold any capturing closure with its checked stack effect. The graph includes each possible closure record and its owned captures. Within a cycle, the record's destructor pointer selects its capture layout. Generated cleanup reads the record directly and preserves each capture's active flag and cleanup order without changing the calling closure's environment.

The worker preserves the cleanup contract of [ADR-0050](0050-destruction-is-lifo.md). It calls each compiler-called cleanup method first, destroys fields in reverse declaration order, and releases parent storage after child cleanup. A cleanup method runs as an ordinary call, so destruction that it starts finishes before it returns.

The buffer starts in the worker frame with room for 16 struct, enum, or closure continuations, so shallow values do not allocate. Growth moves it to a heap buffer of twice the capacity, which the rest of the operation reuses. Each pending struct, enum, or closure level uses 16 bytes, and each pending array uses 24 bytes.

## Considered options

- Call-stack recursion has the lowest cost per node, but a deep value exhausts the 1 MiB return stack and terminates without cleanup.
- Pointer-reversal traversal needs no buffer, but it must temporarily rewrite fields of every recursive layout and complicates cleanup methods that read those fields.
- A separate allocation for each pending node makes allocation the dominant cost.
- Specialized loops for linear chains leave branching and mutually recursive layouts unbounded.

## Consequences

- Recursion through a user-written cleanup method stays on the return stack. Standard collections such as `std::List` destroy their elements in a cleanup method, so a recursive type that keeps its children in a collection is outside this guarantee.
- Destruction of a recursive value costs more per node than a native call and return, because each level stores and reloads its continuation and resumes through an indirect jump.
- A failed buffer allocation terminates the process, like other allocation failures.
- The cycle analysis in the backend must list the same destruction calls as the generated bodies. A missing call keeps destruction correct but lets that call nest on the return stack.
