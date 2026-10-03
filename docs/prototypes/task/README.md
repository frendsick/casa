# Casa documentation

Prototype B: organize pages around what the reader needs to do. Separate a
guided lesson from a task, an explanation, and an exact reference.

## Proposed layout

```text
README.md                         Install and run a first program
docs/
  README.md                       Choose a task or reading path
  tutorials/
    functions-and-lists.md
    command-line-program.md
  how-to/
    change-a-list.md
    handle-errors.md
    use-native-libraries.md
  explanation/
    stack-and-calls.md
    ownership.md
  reference/
    notation.md
    language/
      functions.md
      operators.md
      modules.md
      ...                         Remaining language topics
    list.md
    map.md
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

## Learn or complete a task

| You want to… | Read |
|---|---|
| Learn calls and collections through a complete program | [Functions and lists](tutorials/functions-and-lists.md) |
| Add, read, and remove a list element | [Change a list](how-to/change-a-list.md) |
| Understand operand order | [The stack and calls](explanation/stack-and-calls.md) |
| Decode a declaration or stack effect | [Reference notation](reference/notation.md) |
| Check a method's ownership and failure behavior | [List reference](reference/list.md) |

## Page style

A tutorial gives the reader a working result in a defined sequence. A how-to
page starts with an existing task and supplies only the steps needed for it.
An explanation develops the model behind a rule. A reference states the
contract without requiring a lesson to be read first.

Use these categories when their contents differ. A new method does not require
four new pages. Add a task page when it solves a recurring problem involving
several operations. Keep each contract in the reference and link it from the
other pages.
