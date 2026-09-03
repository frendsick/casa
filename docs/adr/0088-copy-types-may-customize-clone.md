# Copy types may customize Clone
status: superseded by [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md)

A type that implements standard Copy may provide its own ordinary Clone implementation. The Copy implementation supplies fieldwise Clone behavior only as a fallback when no explicit implementation exists; an explicit implementation takes precedence and is not a duplicate-implementation error.

Customized Clone does not affect Copy semantics. Implicit reuse, `dup`, and `over` still perform compiler-validated, allocation-free representation copying. An explicit `.clone` call may allocate, run user code, or choose different domain behavior according to its implementation.

The implementation author is responsible for honoring Clone's documented semantic contract. The compiler continues to enforce type, ownership, borrowing, and safe-code rules, but it does not prove that the returned value is semantically equivalent to the source or that cloning is cheap.

## Consequences

- `derives Copy` plus a handwritten Clone implementation is valid; the handwritten method replaces the fieldwise fallback.
- `derives Clone` and one handwritten Clone customization merge under ADR-0090, with the handwritten method taking precedence.
- Generic `[T: Copy]` code may call `.clone` through the standard supertrait, but that explicit call is not guaranteed allocation-free.
- Code requiring trivial duplication uses Copy operations rather than Clone.
