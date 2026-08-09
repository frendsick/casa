# Borrows convert to raw addresses with `ptr::from_ref`

Casa provides one safe named operation for obtaining the raw address of borrowed storage:

```casa
buffer ptr::from_ref = address
```

`ptr::from_ref` accepts `$T` and returns `ptr`. An owner may lend the required shared borrow automatically, and `mut$T` may weaken to it under the ordinary receiver-capability rule. Casa does not add `ptr::from_mut` because raw `ptr` carries no mutability distinction.

Creating and copying an address does not access memory and is therefore safe. Pointer arithmetic, loads, stores, typed borrow reconstruction, and foreign use retain their existing unsafe requirements.

## Consequences

- The result is a non-owning raw pointer and does not keep its source alive, borrowed, or immovable.
- Moving or destroying the source may leave the raw pointer dangling; merely storing or comparing that pointer remains safe, while later memory access must re-establish validity in unsafe code.
- Safe wrappers accepting `mut$T` may pass its address to an unsafe foreign operation that writes, with the wrapper's exclusive input providing the aliasing contract.
- `ptr::from_ref` performs no allocation, metadata construction, or runtime check.
- General casts and implicit borrow-to-pointer conversions remain absent.
