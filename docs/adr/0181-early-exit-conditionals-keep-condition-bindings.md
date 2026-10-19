# Early-exit conditionals keep condition bindings

An **early-exit conditional** omits `then`: `if condition else body fi`. When
the condition is true, execution continues after `fi`. When it is false, the
`else` body runs. That body must end the path with `break`, `continue`,
`panic`, `return`, or a call to a function that cannot return. A body that can
reach `fi` is a compile-time error.

Bindings created by the condition, including enum `is` pattern bindings, remain
available from `fi` to the end of the enclosing body. The `else` body cannot use
them because the pattern did not match on that path:

```casa
if value std::Option::Some(text) is else
    "missing value" report
    return
fi
text consume
```

The form accepts one condition. It has no `then` branch, `elif` branch, or
inline value form. Any boolean condition is valid, so a condition without
bindings works as a plain exit check.

The form appears directly in a function, root, branch, loop, or match arm body.
A condition, array literal, group, match guard, f-string expression, or
`unsafe` block rejects it. Those sequences end before the enclosing scope, and
an `unsafe` block does not otherwise scope its bindings.

The ownership rules in [ADR-0027](0027-patterns-follow-subject-ownership.md) and
[ADR-0178](0178-patterns-compose-recursively-and-share-arm-bodies.md) do not
change. A successful check transfers non-`Copy` payloads from an owned subject,
so that subject is unavailable after `fi`. A failed check keeps the complete
subject in the `else` body. Shared and exclusive subjects produce borrows with
the subject's capability. `Copy` payload bindings do not consume the subject.

The compiler checks the conditional as an `if` whose only branch holds the rest
of the enclosing body. Scope, cleanup order, and loan joins therefore use the
existing branch rules.

## Considered options

- Keeping `.is_none then` checks followed by `unwrap` checks each value twice
  and adds a panic path that the type checker cannot rule out. The compiler,
  language server, formatter, and standard library use this form widely.
- Matching on the value and returning from the failing arm, as in
  `value match Some(text) => text _ => return end = text`, repeats the binding
  name and needs a separate assignment after `end`.
- Writing `subject Pattern is else body fi` without `if` is ambiguous. A branch
  body can end with an `is` check before the enclosing `else`.
- A new keyword such as `guard` would reserve a name that the compiler sources
  use as an identifier. It would also collide with match arm guard terminology.
- Accepting only pattern conditions adds a rule but does not prevent a harmful
  program. A plain condition already behaves like `if condition ! then body fi`.

## Consequences

- Bindings from a failed check are unavailable in the `else` body. Editor
  completion offers them after `fi`, not in the `else` body.
- The form never produces a value, so the formatter always writes its body on
  separate lines.
- Several early exits in a row nest one branch inside the previous one. Each
  binding is destroyed at the end of the enclosing body, in reverse order of
  creation.
