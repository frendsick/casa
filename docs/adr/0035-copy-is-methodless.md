# Copy is methodless
status: amended by [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md), [ADR-0158](0158-copy-requires-a-raw-value-representation.md), and [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md)
related issue: #314

`Copy` permits implicit binding reuse, `dup`, the copied operand of `over`, and
`copy` from `$T` to owned `T`. It duplicates representation bits, never allocates,
and never invokes user code. There is no method to customize.

Built-in scalars and named function references implement Copy. User structs and
enums opt in only with `derives Copy`. The compiler requires a raw value
representation and fields safe to duplicate, including stored shared borrows
under ADR-0150. Duplicated shared fields preserve their loan origins. Custom
destruction, duplicated owners, and exclusive borrows disqualify the type. An eligible type may omit Copy to represent a unique scalar
resource or state token.

Shared-borrow duplication is a separate rule under ADR-0150. `$T` does not satisfy
Copy or Clone bounds. Exclusive borrows cannot be duplicated. The `str` view is
Copy. `String`, `Bytes`, lists, maps, owned closures, and resource owners are not.

Explicit duplication uses `[T: Clone]` and may allocate or run user code. Stack
duplication never falls back to Clone. Any Copy-to-Clone relationship comes from
the active declaration under [ADR-0080](0080-language-traits-use-minimum-contracts.md).
A derive supplies its complete trait family under ADR-0163.
