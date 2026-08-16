# Borrows distinguish statically disjoint struct fields

Borrow checking tracks named struct-field paths precisely enough to permit simultaneous loans of statically disjoint fields:

```casa
pair.left = left_view
pair.right.clear
left_view.length
```

The shared borrow of `pair.left` does not overlap the exclusive borrow used to mutate `pair.right`. A loan of the complete `pair` still overlaps both fields.

Indexed places remain conservative because two runtime indexes may identify the same element:

```casa
items.get_mut(i) = first
items.get_mut(j) = second # error while first is live
```

## Consequences

- Loan state records static field paths but definite ownership availability remains whole-binding because partial moves are still forbidden.
- Nested named fields are disjoint after their first differing field component.
- Enum payloads, raw-pointer projections, dynamic indexes, and calls returning borrows are not assumed disjoint without an API contract that proves it.
- Collections may later provide safe operations such as `split_at_mut` that construct disjoint borrowed regions internally.
- The implementation performs field-path overlap checks, not general alias or symbolic-index analysis.
