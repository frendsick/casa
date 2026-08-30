# Domain Docs

How to use Casa's domain documentation while exploring the codebase.

## Before domain work

- Read the relevant terms and relationships in `CONTEXT.md`.
- Search `docs/adr/` for decisions that touch the work.
- Treat ADRs as decisions, not implementation status. Verify current behavior in
  code and tests.

## Use the glossary's vocabulary

When your output names a domain concept (in an issue title, a refactor proposal, a hypothesis, a test name), use the term as defined in `CONTEXT.md`. Don't drift to synonyms the glossary explicitly avoids.

If a required concept is absent, use terminology from current code and note the
gap when it affects the work.

## Flag ADR conflicts

If your output contradicts an existing ADR, name the conflict and explain why
the decision should be reconsidered.
