# `copy` intrinsic materializes a borrowed Copy value

Casa adds the compiler intrinsic:

```casa
copy # [T: Copy] $T -> T
```

It produces one owned `T` by performing the same compiler-validated, allocation-free representation copy used by `dup` and `over`, but reads the value through a shared borrow. The borrowed value remains initialized and unchanged.

```casa
fn copied[T: Copy] value:$T -> T {
    value copy
}
```

`mut$T` may call `copy` through an ordinary temporary shared reborrow.

## Consequences

- `copy` is a compiler intrinsic, not a trait method or standard-library function. A generic Casa function could not implement it without already having an operation that materializes `T` from `$T`.
- `dup` retains `[T: Copy] T -> T T`; applying it to `$T` duplicates the borrow because the stack value itself is a shared reference.
- `copy` invokes no Clone implementation, user code, allocator, or destructor.
- Compiler lowering uses `T`'s layout and does not require aggregate padding to contain initialized user-observable bytes.
- Concrete built-in copyable values remain usable without importing the standard library. Generic code names the active compiler-validated Copy trait in its bound.
- `copy` is rejected when the borrowed value is not Copy; use explicit `T::clone` when allocation or type-specific duplication is acceptable.
