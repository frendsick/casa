# Casa manual

Prototype C: keep a small number of manuals with stable section anchors. Read
the language in sequence and use tables to look up library operations.

## Proposed layout

```text
README.md                         Install and run a first program
docs/
  README.md                       Manual index
  handbook.md                     Values, calls, ownership, control flow,
                                  types, traits, and modules
  library.md                      Collections, text, errors, OS, and utilities
  tooling.md                      Compiler, formatter, and language server
  contributing/
    style.md
    compiler-products.md
```

The tree is a proposed layout. The two sample manuals cover only calls and
lists. Contributor records and runnable examples keep their existing homes, as
described in the [comparison](../README.md#conventions-worth-keeping-in-any-option).

## Read the manual

1. [Install Casa](../../../README.md#install).
2. Read [the handbook](handbook.md) for stack evaluation, calls, and notation.
3. Open [the library manual](library.md) for a complete list example and method
   declarations.

For lookup, go directly to [operand order](handbook.md#operand-order),
[stack effects](handbook.md#stack-effects), or
[list operations](library.md#list-operations).

## Page style

Keep explanations short and arrange concepts in dependency order. Use a
complete program to introduce a subject, then tables for the related forms.
Put exceptions directly below the table. Link within the manual when a rule
has already been explained.

Each manual needs a short contents list. Split it when major topics need
independent navigation or changes become hard to review. This option tests
how far a few ordinary Markdown files can go before that split is useful.
