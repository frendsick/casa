# Opaque returned borrows keep the complete input loaned

When a function returns borrows derived from a borrowed input, its public stack effect does not expose which internal fields or regions they reference. The complete input therefore remains loaned until every returned borrow derived from it reaches its last use:

```casa
pair.split = right = left

left.clear
pair.left.length  # error while right remains live
right.clear
```

Although `split` proved that `left` and `right` do not alias each other, callers cannot infer that either output corresponds to a particular field of `pair`. Direct field borrowing retains that static path precision because no opaque call boundary intervenes.

## Consequences

- Multiple returned exclusive borrows may be used together, but their source owner remains unavailable until all expire.
- The rule applies equally to returned shared borrows and aggregates containing borrows.
- The compiler records input origin sets in public function facts, not body-derived field projections.
- APIs needing more source-place precision expose direct field operations or narrower borrowed inputs.
- Casa adds no field-projection, effect, or named-lifetime syntax to function types initially.
