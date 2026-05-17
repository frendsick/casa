# Pitfalls

Known Casa gotchas that aren't yet captured as fixes. Each entry has a one-line
description, a workaround, and a link to the tracking issue. Remove an entry when its
issue closes.

## Type enum methods may consume the value (context-dependent)

Casa enum values (`Type`, `Option[T]`) are usually safe to call methods on multiple
times — `t.format` twice on a named `Type` var works in isolation, for-loop iteration
works, list re-iteration works. But there are reported patterns that crash stage2 with
"unwrap on None" after an enum method call, especially when chained `.get.typ`
extractions feed into multiple methods.

- **Default:** write the cleaner reuse-the-value code.
- **If you hit a stage2 crash** after an enum method call: bind the intermediate
  result to a fresh variable, or fall back to formatting once and threading the
  string through. Then verify with `tests/test_compiler.sh` and `tests/test_examples.sh`.
- No tracking issue yet — root cause unconfirmed. File one if you reproduce a
  specific crash pattern.
