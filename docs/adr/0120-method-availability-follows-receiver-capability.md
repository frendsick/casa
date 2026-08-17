# Method availability follows receiver capability

Method availability is determined by the declared receiver and the capability available at the call site:

| Declared receiver | Owned `T` | Shared `$T` | Exclusive `mut$T` |
|---|---|---|---|
| `self` | allowed; consumes | rejected | rejected |
| `$self` | allowed; shared borrow | allowed | allowed; shared reborrow |
| `mut$self` | allowed; exclusive borrow | rejected | allowed |

These rules apply uniformly to inherent methods, trait methods, operators lowered to methods, and generic calls. They do not inspect the method name or recognize Clone, equality, hashing, ordering, or display specially.

Method lookup first considers the exact value type. If that type has no applicable method, a borrowed value may call a method on the borrowed value's type when the declared receiver permits the access. Type qualification selects that method explicitly.

## Consequences

- A consuming `self` method requires ownership and can never be called through a borrow.
- A `$self` method is callable through owned, shared, or exclusive access; exclusive access is weakened by an ordinary shared reborrow.
- A `mut$self` method is callable through an owner or exclusive borrow, never a shared borrow.
- If `T: Clone`, `mut$T.clone` may resolve to `T.clone` and return an owned `T`; this is ordinary `$self` receiver lookup.
- If `$T` has its own Clone implementation through standard `Copy: Clone`, `$T.clone` selects that exact implementation and returns `$T`. `T::clone` explicitly clones the borrowed value.
- Expected return types never choose a method implementation.
