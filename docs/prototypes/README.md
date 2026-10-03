# Documentation prototypes

Three Markdown options for Casa's user documentation. Each option includes a
proposed directory tree and drafted pages. Open them in a Markdown preview or
read the source directly. No website or documentation generator is required.

These are partial prototypes on `docs/documentation-prototypes`. The existing
documentation remains in place. No option has been selected.

## Compare the options

| Option | Organization | Writing style | Main tradeoff |
|---|---|---|---|
| [A: Topic reference](topic/README.md) | A short guide, language topics, library types, and tools | Explain the rule, show its use, then state the exceptions | Close to the current docs, but the guide and references need clear boundaries |
| [B: Learning and tasks](task/README.md) | Tutorials, how-to guides, explanations, and reference | Give each page one reader goal | More focused pages, but more links and more editorial decisions |
| [C: Compact handbook](handbook/README.md) | A language handbook and a library manual | Teach in reading order, with compact lookup tables | Fewer page changes, but longer files and more reliance on headings |

Each tree shows a possible final layout. Only the linked sample pages exist.
The samples cover the same material: operand order, a named function, and list
creation, insertion, access, and removal. They assume general programming
knowledge and no previous experience with stack-based languages.

Compare the same reading paths:

| Reader's question | A | B | C |
|---|---|---|---|
| How do I call a function? | [Guide](topic/guide.md) | [Tutorial](task/tutorials/functions-and-lists.md) | [Handbook](handbook/handbook.md#call-a-function) |
| Why are the arguments in that order? | [Functions](topic/language/functions.md#operand-order) | [Explanation](task/explanation/stack-and-calls.md) | [Operand order](handbook/handbook.md#operand-order) |
| What does `List.push` consume? | [List reference](topic/library/list.md#push) | [Method contract](task/reference/list.md#push) | [List table](handbook/library.md#list-operations) |
| Can I mutate a list while an element is borrowed? | [Borrowing](topic/library/list.md#borrowing-elements) | [How-to guide](task/how-to/change-a-list.md#finish-reading-before-changing-the-list) | [Borrowing section](handbook/library.md#read-before-mutation) |

## Documentation choices

| Choice | A: Topic reference | B: Learning and tasks | C: Compact handbook |
|---|---|---|---|
| Page opening | Definition or rule | Goal for a task, contract for a reference | Concept in the chapter's reading sequence |
| Examples | One complete example per topic, extra examples for surprising rules | Complete tutorial and task programs, short call forms in reference | A few complete programs followed by compact examples |
| Expected results | Comments for one value, output blocks for several lines | Explicit output after each runnable program | Comments beside short examples, output blocks for complete programs |
| Declarations | Named parameters in reference entries | Exact source declarations in dedicated contract entries | Declarations in a compact table |
| Stack effects | Beside operations where they clarify consumption | A separate field in each sampled method contract | A notation section and operator table, omitted when a declaration already says enough |
| Cross-references | Inline links at prerequisites and exceptions | Links between tutorial, explanation, task, and reference | Mostly section anchors within a manual |
| Ownership and failure | Beside the operation, with one shared explanation | Explicit contract paragraphs | Short table entries expanded immediately below |
| Maintenance | Split a topic when it becomes hard to navigate | Keep procedure, explanation, and contract consistent | Keep headings stable and avoid growing an encyclopedic single file |

Directory structure and page style can be chosen separately. For example, A's
directories can contain B's method contracts without introducing all four
document categories.

## Recommendation to evaluate

Start with A's directory structure and use B's explicit contracts for library
methods with ownership or failure behavior. Casa already has a guide and topic
references. Grouping them by language, library, and tools gives those pages a
clear home without requiring a second set of explanations for every topic.

Keep the guide example-led. Use declarations as the default reference notation.
Add a type-only stack effect when it helps explain operators, callbacks, or
argument consumption. Avoid mechanically repeating both forms for every
method. The samples deliberately show different amounts of repetition so this
choice can be judged directly.

Choose B if readers often arrive with a task such as changing a collection or
handling an error. Choose C if a continuous manual is more useful than many
independently linked pages. These preferences have not been tested with readers.

## Conventions worth keeping in any option

- Use Casa's existing terms: function declaration, method declaration, function
  type, and stack effect. They describe different things in the
  [glossary](../../CONTEXT.md#documentation-terminology).
- Define stack notation once, then link it from lookup pages. Inputs use
  consumption order. Outputs use push order. A stack snapshot has its top on
  the right. Do not silently change between these conventions.
- Show a call whenever parameter order could be misread. Two identical input
  types do not explain which value becomes the first argument.
- Make runnable examples complete, including imports, and show the command and
  expected result. Label declaration excerpts and incomplete call forms as such.
- Give each rule one reference home. A guide can briefly restate a rule needed
  for its example, then link the full contract.
- Put ownership, mutation, failure, and preconditions next to the operation
  they qualify. A link alone is insufficient for an operation that can terminate
  the program.
- Link to a specific prerequisite or next task. Avoid repeating a broad
  “See also” list under every heading.
- Keep complete applications in the existing [examples](../../examples/README.md).
  Use small inline examples to explain one behavior.
- Keep compiler design, ADRs, benchmarks, and contributor instructions separate
  from the path for learning the language. Existing `docs/adr/`, `docs/agents/`,
  `docs/audits/`, `docs/benchmarks/`, and `CONTEXT.md` remain outside these samples.

## Current consistency issue

The [collection reference](../collections.md) still shows a selective import
and unqualified library names. The [module reference](../modules.md#qualified-names-and-module-identity)
states that selection clauses are unsupported and imported declarations require
qualification. The current [sorting example](../../examples/sorting.casa)
uses `std::List::from_array`.

The prototypes use qualified names and method declarations checked against
[lib/std.casa](../../lib/std.casa). This discrepancy shows why moving files
alone will not improve correctness. Updating the existing pages belongs in the
follow-up work after choosing a documentation direction.

## Evaluate before migrating

Use each option to find the first argument of `subtract`, the ownership of a
value returned by `get`, and the behavior of `pop` on an empty list. Compare how
much scrolling and prior reading each answer needs.

Also try making one hypothetical documentation change: change a method's return
type. Count the declarations, effects, examples, and prose that would need an
edit. This exposes the maintenance cost of repeated contract information.

An adoption change should map all current pages to their destinations, update
relative links and inbound references, and retain useful existing detail. These
samples are not a proposal to delete the topics they omit.
