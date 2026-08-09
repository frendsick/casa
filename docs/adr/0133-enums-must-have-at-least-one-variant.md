# Enums must have at least one variant

Every enum declaration contains at least one variant:

```casa
enum Impossible { } # error
```

An empty enum would be an uninhabited type equivalent to the previously rejected `Never` type. Casa does not introduce that control-flow and exhaustiveness behavior indirectly through enum syntax.

## Consequences

- The parser or declaration checker reports an empty enum at its declaration.
- Every valid enum is inhabited whenever at least one variant's payload types are inhabited.
- Exhaustiveness checking never needs a vacuous empty-enum case.
- Empty structs remain valid one-value marker types with the one-byte minimum representation from ADR-0132.
- A future demonstrated need for uninhabited types requires revisiting the explicit `Never` decision rather than exploiting an empty enum loophole.
