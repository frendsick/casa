# Polymorphic recursion is rejected

Recursive calls within one function-call cycle must preserve their generic type arguments. Ordinary recursion at the same concrete specialization is allowed; a recursive call that changes a participating type argument is a compile-time error.

```casa
fn recurse[T] value:T {
    value Option[T]::Some recurse[Option[T]] # error
}
```

Allowing an unbounded type-changing cycle under monomorphization would require erased runtime type descriptors and method dictionaries, a JIT, or an arbitrary specialization limit. Casa chooses none initially. Rejecting polymorphic recursion keeps binaries finite and preserves the direct monomorphized model from ADR-0069.
