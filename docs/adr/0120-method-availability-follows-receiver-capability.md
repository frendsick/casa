# Method availability follows receiver capability
status: amended by [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md)
related issue: #314

Method availability is determined by the declared receiver and the capability available at the call site:

| Declared receiver | Owned `T` | Shared `$T` | Exclusive `mut$T` |
|---|---|---|---|
| `self` | allowed, consumes | rejected | rejected |
| `$self` | allowed, shared borrow | allowed | allowed, shared reborrow |
| `mut$self` | allowed; exclusive borrow | rejected | allowed |

These rules apply uniformly to inherent methods, trait methods, operators lowered to methods, and generic calls. They do not inspect the method name or recognize Clone, equality, hashing, ordering, or display specially.

Method lookup first considers the exact value type. If that type has no applicable method, a borrowed value may call a method on the borrowed value's type when the declared receiver permits the access. Type qualification selects that method explicitly.

## Consequences

- If `T: Clone`, `mut$T.clone` may resolve to `T.clone` and return an owned `T`; this is ordinary `$self` receiver lookup.
- Shared-borrow duplication does not confer Copy or Clone under ADR-0150. When `T: Clone`, `$T.clone` can use `T`'s `$self` method and return owned `T`. `T::clone` selects it explicitly.
- Expected return types never choose a method implementation.
