# Immutable globals may be public
status: superseded by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Immutable globals follow the same private-by-default visibility rules as other declarations and may be marked `pub`. Consumers observe a public non-`Copy` global through its program-lifetime shared borrow; an owned value is available only through the existing contextual `Copy` rule.

Forbidding public globals would require trivial accessor functions without improving safety now that mutable globals do not exist. Constants remain the preferred API for compile-time values, while public immutable globals support larger runtime-built read-only data.
