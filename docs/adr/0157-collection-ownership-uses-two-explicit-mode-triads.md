# Collection access and iteration expose ownership
related issue: #369

Checked collection access borrows elements. Removal transfers ownership where
its return type supplies an owned value. Iteration borrows the source and its
elements. These distinctions follow
[ADR-0013](0013-affine-ownership-with-automatic-storage.md) and
[ADR-0014](0014-explicit-unsafe-boundary.md).

`List.get` and `List.get_ref` return `$T`. `List.get_mut` returns `mut$T`.
These operations terminate on an invalid index. `List.remove` returns `Option[T]`.

`Map.get`, `Map.get_mut`, and `Map.remove` return `Option[$V]`,
`Option[mut$V]`, and `Option[V]`. Missing keys return `Option::None`.
`Map.iter` yields `Pair[$K $V]`. Stored keys receive no mutable access because
changing equality or hashing could invalidate their placement.

`Set.has` observes membership. `Set.remove` removes and destroys a value without
returning it. `Set.iter` yields `$K`. It has no mutable element access.
To change equality or hashing, remove the value and insert a replacement.

List traversal preserves index order. Map and Set traversal order is unspecified.
All three borrow their elements during iteration. They provide no `iter_mut`
or `into_iter` methods.
