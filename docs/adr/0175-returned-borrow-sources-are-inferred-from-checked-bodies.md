# Returned-borrow sources are inferred from checked bodies

Casa infers a source summary for each output from the checked function body.
The summary includes every returning path, including early returns. An aggregate
output keeps the union of the dependencies of its contents. Separate outputs
have separate summaries. This amends [ADR-0046](0046-returned-borrows-use-all-compatible-inputs.md).

Named calls and function values with inferred types retain these summaries.
Moving a function value or selecting between function values preserves every
possible source. The relationship does not depend on optimization or inlining.
Generic bodies are checked symbolically once. Calls substitute input origin and
cleanup dependencies into the cached summary.
Copied borrow payloads retain their stored sources. Borrowing an owned field,
including a function field, also retains the storage that contains it. Borrowed
pattern bindings keep the original source owners when their local scope ends.

A general, explicitly written `fn[...]` contract erases this precision. Calls
through that contract use all compatible inputs, including borrowed payloads
and capture dependencies. A known target does not recover the erased summary.
Calls checked only against a trait requirement use its conservative contract.
A known concrete implementation can use its checked body. Inference through
arbitrary higher-order contracts is outside this decision.

Recursive call cycles retain conservative sources. This avoids making a
borrowing contract depend on which declaration the checker visits first.
Narrower recursive relationships require a separate proof mechanism before
callers can rely on them. Loops that change symbolic input or cleanup
dependencies also retain the full contract, including possible callback captures.

Source inference selects complete input owners. It does not expose fields or
regions across a call, so [ADR-0108](0108-opaque-returned-borrows-keep-the-complete-input-loaned.md)
still applies. Local-owner escape and exclusive results supported only by
shared inputs remain errors. Unsafe operations contribute the origins that
the checker proves. Inference cannot discard them. A library can narrow a raw
conversion through a receiver-only projection function whose unsafe contract
requires a valid address within the receiver's storage.

The inferred summary is part of the public borrowing contract. A body change
that introduces another possible source can break callers without changing the
written signature. Editor hovers expose inferred input sources, and borrow
conflicts identify the owners that remain loaned.

## Rationale

Temporary map and JSON lookup keys must be reusable while a result belonging
to the collection remains live. Requiring explicit source annotations would
add syntax to ordinary composition. Body inference supplies that relationship
while keeping explicit function types and abstract trait requirements usable
without a higher-order lifetime system.
