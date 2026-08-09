# Patterns follow subject ownership

An owned `match` subject is consumed. In the selected arm, non-`Copy` bound fields move into their bindings, `Copy` fields copy, and omitted fields are destroyed. The complete subject is therefore handled exactly once even when a struct pattern names only some fields. Matching a borrowed subject binds field borrows of the corresponding mutability instead.

Conditional `is` is observational: it borrows its subject for the conditional region rather than consuming it. Non-`Copy` payload bindings are branch-scoped borrows, while `Copy` payloads are copied. Use `match` when an arm must take payload ownership. A temporary tested with `is` remains alive through the conditional region and is then destroyed.

## Considered options

- Making both `match` and `is` consume is uniform, but turns a lightweight conditional test into an ownership transfer and makes the original value unavailable afterward.
- Making both operations borrow preserves values, but forces a separate extraction mechanism whenever code needs to own an enum payload.
- Adding `ref`, `move`, or similar modifiers to individual patterns exposes more control, but complicates common destructuring before demonstrated need.
- Letting the subject type determine binding ownership gives owned extraction through `match` and concise observation through `is` without new syntax.

## Consequences

- A wildcard arm over an owned subject destroys that subject. A selected partial struct pattern destroys every omitted field.
- Matching `$T` produces shared field borrows. Matching `mut$T` may produce exclusive borrows to the selected disjoint fields, all limited to the arm.
- `is` leaves an owned variable available after the conditional, but any non-`Copy` payload binding cannot escape its branch-scoped borrow.
- A guard over an owned subject inspects prospective bindings through temporary borrows. It may not consume them. Moves and destruction commit only after the guard succeeds; a failed guard leaves the complete subject available to later arms.
- Pattern lowering may no longer duplicate or load owned payload words blindly. Shared operation semantics records the borrow, copy, move, and destruction behavior before bytecode lowering.
- Casa initially forbids moving individual fields out of an owner outside complete consuming destructuring.
