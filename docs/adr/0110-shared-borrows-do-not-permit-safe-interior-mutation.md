# Shared borrows do not permit safe interior mutation

A shared `$T` borrow does not permit safe code to mutate the borrowed storage or derive an exclusive borrow from it:

```casa
fn bump counter:$Counter {
    1 += counter.value # error
}

fn bump counter:mut$Counter {
    1 += counter.value
}
```

This is a memory-access guarantee, not a claim that every shared method is free of external effects. Ordinary mutation of owned state uses an owner or `mut$T`.

## Consequences

- Projecting through `$T` yields shared access to owned fields, recursively.
- A method taking `$self` cannot mutate the receiver's ordinary fields.
- The compiler may rely on borrowed storage remaining unchanged through safe aliases while a shared loan is live.
- Casa initially has no `Cell`, atomic, lock, or other safe interior-mutability primitive.
- If a concrete need appears, one explicit privileged container may define controlled interior mutation without weakening `$T` generally.
