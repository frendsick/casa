# Unsafe code must preserve live-borrow invariants

Unsafe raw-memory operations must preserve the lifetime, validity, and aliasing invariants of every live Casa borrow. Mutating borrowed storage through a raw pointer while a shared borrow is live is undefined behavior:

```casa
value = view

unsafe {
    address 42 store64 # undefined behavior if address points into value
}

view.inspect
```

An `unsafe` block permits operations whose proof the compiler cannot perform. It does not suspend the invariants on which safe code and optimization rely.

## Consequences

- Raw writes must not overlap any live shared borrow or incompatible exclusive borrow.
- Forming a typed borrow from `ptr` requires valid, aligned, live storage with the promised aliasing for the complete borrow duration.
- Foreign code called through unsafe FFI has the same obligation for Casa memory it can access.
- The runtime performs no raw-pointer alias checks.
- A safe wrapper containing unsafe operations must establish these conditions before exposing an ordinary safe contract.
