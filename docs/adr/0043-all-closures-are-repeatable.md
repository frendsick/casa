# All closures are repeatable

Casa has no single-use closure capability. Every closure has a repeatable `fn[...]` type. A closure definition is rejected when any invocation path could move or destroy a captured non-`Copy` value without restoring that capture before the invocation returns.

Closures may consume their explicit arguments because each invocation receives new arguments. They may inspect or mutate captured owners and may temporarily move a capture when every continuing path reinitializes it. Destroying the closure value itself destroys its owned captures normally; the restriction applies to invoking the closure.

```casa
resource = open_resource

move { resource.inspect } # valid: capture remains owned
move { resource.close }   # error: invocation consumes a capture
```

One-time ownership transfer uses an ordinary consuming function or method whose ownership appears in its parameters:

```casa
fn close_resource resource:Resource {
    resource.close
}
```

## Considered options

- Inferring a hidden single-use capability preserves capture-consuming closures, but makes higher-order contracts and stored callbacks carry an additional call mode.
- An explicit `once fn[...]` type makes that mode visible, but adds syntax and callable subtyping before Casa has an API that requires deferred one-shot jobs.
- Passing owned state as function arguments keeps consumption in ordinary ownership contracts and leaves one function-value model.
- Rejecting capture consumption is restrictive but simple, predictable, and compatible with adding a one-shot type later if real workloads need it.

## Consequences

- `{ ... }` borrows captures and `move { ... }` owns captures; both forms are repeatable.
- `fn[...]` is the only function-value type. Casa adds no `once fn[...]`, `FnOnce`, or implicit one-shot capability.
- `exec` borrows a callable for the invocation rather than consuming the callable value. Shared and mutable callable borrows follow the `$fn[...]` and `mut$fn[...]` rules.
- Capture availability is checked with the same whole-binding control-flow analysis used elsewhere. A possible un-restored move on any returning path rejects the closure.
- Standard-library callback APIs do not need callable-capability parameters or inference.
- If Casa later needs heterogeneous deferred jobs that bundle arbitrary owned state with consuming code, that concrete use may justify a distinct one-shot callable type.
