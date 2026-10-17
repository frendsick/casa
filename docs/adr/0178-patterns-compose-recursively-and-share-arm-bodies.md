# Patterns compose recursively and share arm bodies

Enum payloads and struct fields accept nested patterns in `match` and `is`.
A nested pattern can bind a name, ignore a value with `_`, test a literal, or
inspect another enum variant or struct. Enum payload positions retain their
existing order. Omitted struct fields and trailing enum payload positions match any
value in those positions.

A match arm can join alternatives with `|`, as in
`Option::Some(Result::Ok(value)) | Option::Some(Result::Error(value)) => value`.
Alternatives belong to the arm, outside payload and field patterns. Each
alternative must bind exactly the same names with the same types and subject
capabilities. Names can occur at different positions. A name occurs at most
once in each alternative.

Alternatives are tested from left to right. The first matching alternative
supplies the bindings for the arm's single optional guard. If the guard fails,
execution continues at the next arm. It does not retry the remaining
alternatives. Guards do not contribute to exhaustiveness or coverage of later
arms. An alternative that adds no values beyond earlier unguarded arms or
previous alternatives in its own arm is rejected as redundant.

Coverage follows constructors and their payloads recursively. Enum variants,
structs, and both boolean values form finite constructor sets. Integer,
character, and text literals require a wildcard or binding to cover the
remaining values. Coverage is checked across all payload and field positions,
so independent coverage of each position does not imply coverage of their
combinations.

The ownership rules in [ADR-0027](0027-patterns-follow-subject-ownership.md)
apply at every depth. Tests and guards inspect the intact subject. Bindings,
moves, wrapper destruction, and destruction of ignored fields commit only on
success. A failed nested `is` check preserves the complete subject. Borrowed
subjects produce bindings with the subject's capability, except for `Copy`
values, which remain values.

## Considered options

- Keeping one-level patterns requires another control form for each nested
  payload and repeats arm bodies for alternatives. Existing compiler code
  contains both forms of repetition.
- Desugaring alternatives into separate arms repeats guard and body checking
  and obscures their shared binding contract.
- Allowing different bindings across alternatives leaves names undefined in
  some executions of the shared body.
- Adding alternatives inside every nested position increases syntax and
  coverage complexity. Complete arm alternatives cover the demonstrated need.

## Consequences

- One recursive representation supports syntax, type checking, ownership,
  coverage, editor bindings, and code generation.
- Structural coverage replaces string keys based only on the outer variant.
- Pattern depth can increase checking work, but coverage specializes only the
  constructors present in the patterns rather than enumerating runtime values.
- `is` retains its enum-root syntax and branch-scoped binding rules.
- A bitwise `|` after a literal at the end of an unbraced arm body can be
  ambiguous with the next arm's alternatives. Such bodies require braces.
