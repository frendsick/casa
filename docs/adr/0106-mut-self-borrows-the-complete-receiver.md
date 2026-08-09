# `mut$self` borrows the complete receiver

A method whose receiver is `mut$self` exclusively borrows the complete receiver, even when its current body mutates only one field:

```casa
pair.left = left_view
pair.clear_right          # error: clear_right takes mut$self
left_view.length
```

The public stack effect permits the method to mutate, replace, or otherwise invalidate any field. Callers therefore cannot combine it with an outstanding field borrow. Calling a method directly on the disjoint field retains the narrower loan:

```casa
pair.left = left_view
pair.right.clear
left_view.length
```

## Consequences

- Borrow checking depends on the declared receiver type rather than inspecting a method body at each call site.
- Changing a method implementation without changing its stack effect cannot invalidate callers' ownership analysis.
- Generic calls, function pointers, imported methods, and default methods obey the same rule.
- Casa adds no field-effect annotations. APIs that need disjoint access expose the relevant fields or accept narrower borrowed parameters.
