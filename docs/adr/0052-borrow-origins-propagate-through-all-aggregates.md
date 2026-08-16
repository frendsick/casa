# Borrow origins propagate through all aggregates

Every struct and enum may contain shared or exclusive borrows. The compiler propagates the contained origin sets through construction, field access, patterns, calls, and returns; an aggregate cannot outlive any owner from which one of its borrows originates.

```casa
struct View[T] {
    first:  $T
    second: $T
}

fn view_pair[T] first:$T second:$T -> View[T] {
    second first View[T]
}
```

The returned `View[T]` is conservatively tied to both compatible inputs. Returning an aggregate containing a borrow of a local owner is rejected. A contained exclusive borrow makes the aggregate non-`Copy`; shared-borrow fields follow the ordinary structural `Copy` eligibility rules.

`Option[$T]`, `Result[$T E]`, and user-defined aggregates use exactly the same mechanism. `Option` and `Result` remain ordinary library enums with no compiler-known ownership behavior. Because safe collection access already requires `Option[$T]` and `Option[mut$T]`, restricting other aggregates would add a special case without removing the need for aggregate origin tracking.

Named lifetime parameters remain deferred. Function-local origin sets and the conservative returned-borrow rule are sufficient for the initial model, and their compile-time cost remains covered by the compiler self-compilation benchmark.
