# Ordering derives Eq and Copy

The standard Ordering enum initially derives only Eq and Copy:

```casa
enum Ordering derives Eq Copy {
    Less
    Equal
    Greater
}
```

Eq satisfies PartialEq and supplies equality hooks. Copy makes the small payload-free enum allocation-free to duplicate and, through the standard Copy supertrait, gives it Clone behavior. Ordering does not initially derive Ord or Hashable because comparison dispatch only needs its variants to represent another type's comparison result.

Additional trait implementations may be added when an API needs to order Ordering values themselves or use them as map keys.
