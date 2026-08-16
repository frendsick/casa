# Composition-first language design

Casa prioritizes **Composition-first programming**: programs are built from small, statically typed functions composed through the value stack. New features should improve that style or its developer experience without making the language substantially less minimal or compilation substantially slower.

Feature work does not require evidence of existing user demand. Casa is a hobby language, so coherent exploration is sufficient when a feature fits the language. Source compatibility may be broken while the ecosystem has one known user; older releases preserve older behavior.

## Considered options

- General-purpose feature parity would make Casa more familiar, but would add constructs already covered by stack composition, functions, traits, enums, and libraries.
- Requiring repeated production evidence would keep the language smaller, but would work against exploratory language development.
- Permanent source compatibility would reduce migration work, but would preserve early mistakes while changing them is still cheap.

## Consequences

- Library functions, traits, and parser desugaring remain preferable to new syntax when they provide equally good composition and developer experience.
- Features may be added speculatively, but must suit Casa rather than copy another language by default.
- Compile-time cost and language complexity are explicit design costs.
- Redundant features may be removed with a documented migration path.
