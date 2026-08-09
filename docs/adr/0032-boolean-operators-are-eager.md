# Boolean operators are eager

`&&` and `||` are ordinary eager stack operations with stack effect `bool bool -> bool`. Both operands are evaluated before the operator executes. Casa does not reinterpret these postfix operators as short-circuit syntax and does not add implicit expression boundaries or closure-taking operator variants.

Code requiring conditional evaluation uses `if`/`else`, making the skipped branch explicit. A named higher-order helper may be added later only if real composition pipelines repeatedly need delayed boolean evaluation.

## Considered options

- Making `&&` and `||` short-circuit matches many infix languages, but the right operand has already executed before a postfix operator receives it.
- Making the second operand a closure permits laziness, but changes the operators' stack effects and adds closure ceremony to ordinary boolean combination.
- Adding separate lazy operators expands syntax for behavior already expressed by control flow.
- Keeping eager operators follows visible stack evaluation and requires no compiler control-flow lowering.

## Consequences

- Documentation and diagnostics must not describe `&&` or `||` as short-circuiting. Examples involving guards use `if` when evaluating the second condition may fail, mutate state, allocate significantly, or otherwise need to be skipped.
- The operators remain constant-time boolean operations in bytecode and add no compile-time analysis.
- Refactoring an infix-language condition into Casa must preserve conditional evaluation explicitly rather than translating operator spelling mechanically.
