# Casa documentation

Prototype A: organize documentation by subject. Keep a short learning guide and
give each language topic, library type, and tool its own reference page.

## Proposed layout

```text
README.md                         Install and run a first program
docs/
  README.md                       Documentation index
  guide.md                        First concepts and a complete program
  language/
    values.md
    operators.md
    functions.md
    ownership.md
    control-flow.md
    structs.md
    enums.md
    traits.md
    modules.md
    intrinsics.md
  library/
    README.md                     Library index
    list.md
    map.md
    text.md
    errors.md
    os.md
    ...                           Remaining library topics
  tools/
    compiler.md
    formatter.md
    language-server.md
  contributing/
    style.md
    compiler-products.md
```

The tree is a proposed layout. The links below open the drafted sample pages.
Contributor records and runnable examples keep their existing homes, as
described in the [comparison](../README.md#conventions-worth-keeping-in-any-option).

## Start here

[Install Casa](../../../README.md#install), then read the
[guide](guide.md). It introduces operand order and calls through two small
programs.

## Look up a rule

| Topic | Read |
|---|---|
| Parameter order, outputs, and stack effects | [Functions](language/functions.md) |
| List insertion, borrowing, and removal | [List](library/list.md) |

## Page style

Start a reference section with its rule or declaration. Follow it with a short
example and any ownership or failure conditions. Use tables for related
operations that fit on one line. Give longer contracts their own headings.

The guide owns the reading sequence. Reference pages own the complete rules.
Link to another page when the reader needs a prerequisite or a related contract.
Split a file when readers need independent entry points, rather than assigning
one file to every function.
