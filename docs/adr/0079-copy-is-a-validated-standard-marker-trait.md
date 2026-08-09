# Copy is a validated standard marker trait

`Copy` is declared as a methodless standard-library marker trait. Its minimum declaration is:

```casa
trait Copy { }
```

The compiler integrates the canonical Copy declaration with implicit binding reuse, `dup`, and `over`. It also validates every Copy conformance: the representation must be safely duplicable without allocation or user code, every owned field must be Copy, and the type must not contain an exclusive borrow, owned indirection, or custom destruction.

User-defined structs and enums may establish the same conformance either inline or in an ordinary empty implementation:

```casa
struct Point derives Copy {
    x: i64
    y: i64
}

struct Size {
    width: i64
    height: i64
}

impl Size: Copy { }
```

Both forms run identical validation. `derives Copy` is the colocated shorthand; it does not define a second kind of capability. The standard declaration names Clone as a supertrait, so either form supplies missing fieldwise Clone behavior; alternative declarations follow their own visible supertraits.

## Consequences

- `[T: Copy]` uses ordinary trait-bound and supertrait machinery. Any additional requirement is visible in the active declaration.
- Selectively importing Copy also imports any declared supertrait dependencies.
- Built-in scalars, shared borrows, raw pointers, and named function references receive compiler-provided conformance to the canonical marker.
- A freestanding program may supply the reserved Copy declaration itself, subject to the minimum-contract rule in ADR-0080.
- The unavoidable compiler integration is limited to eligibility validation and the operations whose semantics depend on bitwise copying.
