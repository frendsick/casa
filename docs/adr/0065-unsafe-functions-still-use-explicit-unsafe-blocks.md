# Unsafe functions still use explicit unsafe blocks

An `unsafe fn` declares that its caller must uphold additional invariants, but its body does not implicitly become an unsafe context. Each unchecked operation remains inside an explicit lexical `unsafe` block.

```casa
unsafe fn read_u64 address:ptr -> u64 {
    unsafe {
        address load64
    }
}
```

Unsafe blocks may consume and produce ordinary stack values. They enable only designated unsafe operations; type, ownership, borrowing, initialization, control-flow, and stack-effect checking remain active. Separating the caller contract from the implementation's exact trusted operations adds minimal ceremony while keeping audits local.
