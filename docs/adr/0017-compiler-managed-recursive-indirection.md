# Recursive ownership uses compiler-managed indirection

Casa permits recursive owned values without exposing `Box`, allocator selection, or an `indirect` field modifier. When an otherwise finite recursive struct or enum needs indirection, the compiler inserts it as a storage detail. Absence remains explicit through `Option`; the existing `0 (StructName)` null sentinel is removed.

## Considered options

- Requiring `Box[T]` makes allocation and layout visible, but makes users choose storage representation for ordinary recursive data.
- Requiring an `indirect` modifier exposes the cost without exposing an allocator, but still adds syntax for a representation the compiler can determine mechanically.
- Compiler-managed indirection keeps recursive ownership in the value model and preserves freedom to select stack, internal-region, or heap placement.
- Accepting recursive definitions with no terminating case gives them finite physical layout after indirection, but no finite safe value can be constructed.

## Consequences

- `Option[Node]` and recursive enum variants may own recursive children without another source-level wrapper.
- Each recursive edge has one owner. Moving the root transfers the complete structure, and destroying it destroys every descendant exactly once.
- Safe code cannot construct ownership cycles. Cyclic graphs use one owner plus indices or IDs; raw pointers remain the unsafe escape hatch.
- A recursive definition with no terminating case, such as `struct Impossible { next: Impossible }`, is rejected with a diagnostic suggesting `Option` or an enum leaf variant.
- Declaration validation finds inhabited types as one least fixed point, so direct and mutual recursion use the same rule before layout and destruction generation.
- Casa does not add a `Never` type; there is no current use that justifies it.
- Recursive indirection makes the owning type non-Copy, but finite recursive types may derive fieldwise Clone under ADR-0093.
- Finite recursive types may also derive comparison and hashing under ADR-0094; their initial generated operations and destruction may use call-stack recursion.
