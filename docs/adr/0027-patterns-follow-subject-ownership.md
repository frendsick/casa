# Patterns follow subject ownership

An owned `match` subject is consumed. In the selected arm, non-`Copy` bound fields move into their bindings, `Copy` fields copy, and omitted fields are destroyed. The complete subject is therefore handled exactly once even when a struct pattern names only some fields. Matching a borrowed subject binds field borrows of the corresponding mutability instead.

Conditional `is` follows the same payload capability rule. On a successful check, an owned subject transfers non-`Copy` payloads into owned branch bindings. A shared subject binds shared payload borrows. An exclusive subject binds exclusive payload borrows. `Copy` payloads remain values and do not consume the subject.

The transfer is conditional. A failed check keeps the complete subject. At `fi`, ownership joins across continuing paths. A successful path that returns does not make the subject unavailable on the unmatched path. A temporary tested with `is` is destroyed on failure. On success, its wrapper is destroyed and its non-`Copy` payloads move into the branch bindings.

## Considered options

- Always consuming an `is` subject is uniform, but consumes values for variant-only checks and `Copy` payloads that need no transfer.
- Always borrowing for `is` preserves values, but forces a separate extraction mechanism whenever a branch needs to own an enum payload.
- Adding `ref`, `move`, or similar modifiers to individual patterns exposes more control, but complicates common destructuring before demonstrated need.
- Letting the subject capability and payload type determine binding ownership gives conditional extraction without new syntax.

## Consequences

- A wildcard arm over an owned subject destroys that subject. A selected partial struct pattern destroys every omitted field.
- Matching `$T` produces shared field borrows. Matching `mut$T` may produce exclusive borrows to the selected disjoint fields, all limited to the arm.
- A variant-only `is` check and an `is` binding of only `Copy` payloads leave an owned subject available.
- An owned `is` subject with a non-`Copy` payload is consumed only on a successful path. The owner is unavailable after `fi` if any continuing successful path reaches the join.
- Shared and exclusive `is` subjects remain available. Their non-`Copy` payload bindings are branch-scoped borrows with the subject's capability.
- A guard over an owned subject inspects prospective bindings through temporary borrows. It may not consume them. Moves and destruction commit only after the guard succeeds; a failed guard leaves the complete subject available to later arms.
- Pattern lowering may no longer duplicate or load owned payload words blindly. Shared operation semantics records the borrow, copy, move, and destruction behavior before bytecode lowering.
- Casa initially forbids moving individual fields out of an owner outside complete consuming destructuring.
