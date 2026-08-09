# Escaping raw mutable borrows require exclusive input origins

An escaping `mut$T` formed with `ptr::as_mut[T]` must be anchored to one or more compatible exclusive borrowed inputs:

```casa
fn first_mut self:mut$Buffer -> mut$u8 {
    unsafe {
        self.data ptr::as_mut[u8]
    }
}
```

A shared input cannot anchor an exclusive result:

```casa
fn first_mut self:$Buffer -> mut$u8 { # error
    unsafe {
        self.data ptr::as_mut[u8]
    }
}
```

Allowing the latter would let a safe caller mutate storage while shared aliases remain live. The unsafe operation can establish raw validity and projection facts, but cannot upgrade the capability declared by the function input.

## Consequences

- Only `mut$` inputs are compatible inferred origins for an escaping `mut$` result.
- A `mut$` input may anchor either an exclusive result or a weakened shared result.
- `$` inputs may anchor only shared results.
- Unsafe blocks and unsafe functions retain ordinary ownership-type checking.
- Raw mutable access without an exclusive input may be used locally under unsafe obligations, but cannot escape as a typed exclusive borrow.
