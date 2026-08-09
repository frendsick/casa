# Exclusive borrows implicitly reborrow as shared

An available `mut$T` may implicitly produce a shorter `$T` reborrow wherever a typed context requires shared access:

```casa
fn freeze value:mut$Buffer -> $Buffer {
    value
}
```

The original exclusive borrow is suspended until the shared reborrow and every value containing it reach their last use. This is a reborrow, not a mutation-capability transfer or runtime conversion.

## Consequences

- Calls, returns, assignments with an explicit expected type, and aggregate construction use the same weakening rule.
- `mut$T` never becomes `$T` by duplicating the exclusive reference; the derived shared loan is tracked as its child.
- `$T` cannot implicitly or explicitly become `mut$T` in safe code.
- Once all derived shared reborrows expire, the original exclusive borrow becomes usable again if it was not otherwise moved.
- Borrow weakening emits no runtime operation and adds no analysis beyond ordinary reborrow tracking.
