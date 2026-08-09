# Calls implicitly reborrow

When a call parameter expects `$T` or `mut$T`, an available owner or borrow is reborrowed for the call instead of being moved. An owned `T` may lend either form, `mut$T` may lend a shorter shared or exclusive borrow, and `$T` may lend another shared borrow.

```casa
fn process items:mut$List[i64] {
    items inspect
    items append_zero
    items append_zero
}
```

The original owner or borrow becomes usable again after the reborrow's last use. If the callee returns a borrow derived from the parameter, that returned value extends the reborrow according to the ordinary borrow-origin rules. Assignments and returns still move an exclusive borrow when their destination expects `mut$T`; automatic reborrowing is a call-boundary rule.

This keeps common mutation and observation composable without explicit reborrow syntax. The compiler records a child loan in the existing function-local ownership state and performs no whole-program analysis.
