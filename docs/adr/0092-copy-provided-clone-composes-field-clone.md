# Copy-provided Clone composes field Clone

When the standard Copy implementation must satisfy its Clone supertrait, the generated aggregate Clone implementation clones each field or active enum payload through its Clone method. It does not raw-copy the complete aggregate representation.

A field may itself be Copy while providing customized Clone behavior. Fieldwise generation respects that customization and allows explicit aggregate cloning to allocate or run user code. Implicit aggregate Copy, `dup`, and `over` remain raw, compiler-validated, and allocation-free.

An explicit Clone implementation on the aggregate still overrides the generated fallback under ADR-0088 and ADR-0090.

## Consequences

- Clone customization composes through structs and enum payloads.
- A Copy aggregate's `.clone` is not guaranteed allocation-free; only Copy operations have that guarantee.
- Standard scalar Copy types use trivial Clone implementations because they contain no customizable fields.
- Copy eligibility remains based solely on representation-safe field copying and is unaffected by Clone cost or behavior.
