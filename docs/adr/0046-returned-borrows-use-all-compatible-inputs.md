# Returned borrows use all compatible inputs

When a function returns a borrow that may originate from more than one borrowed input, Casa permits the function without named lifetime syntax. The returned borrow is conservatively tied to every compatible borrowed input in the function contract.

```casa
fn choose[T] condition:bool left:$T right:$T -> $T {
    if condition then left else right fi
}
```

At a call, every possible source owner must remain alive and cannot be mutably accessed until the returned shared borrow's last use. A mutable returned borrow similarly keeps every possible source exclusively loaned.

## Considered options

- Rejecting multiple possible sources keeps origin inference trivial, but forbids ordinary functions such as choosing the minimum of two borrowed values.
- Named lifetime parameters express precise relationships, but add syntax and a lifetime-level generic system before conservative inference proves insufficient.
- Inferring the exact returned source for each function implementation improves precision, but function values would need extra origin metadata and indirect calls would still need a conservative contract.
- Tying the output to every compatible borrowed input is safe, syntax-free, and works uniformly for direct and indirect calls.

## Consequences

- A returned borrow with one compatible input is tied to that input.
- With multiple compatible inputs, all are considered possible sources even when a particular implementation always returns only one.
- Returning a borrow derived from a local owner remains a compile-time error.
- Function values use the same type-based conservative relationship and need no additional lifetime metadata.
- More precise source annotations remain deferred until real code demonstrates that the conservative loans are too restrictive.
- The analysis is a small input-origin set propagated through existing borrow and control-flow checking; its compile-time effect must remain covered by the compiler self-compilation benchmark.
