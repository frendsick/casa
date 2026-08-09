# Local bindings cannot shadow globals

A local, parameter, pattern binding, or closure parameter cannot use the name of a visible immutable global. The declaration is rejected with a diagnostic suggesting a different local name.

```casa
global LIMIT 100

fn calculate {
    20 = LIMIT # error: shadows global LIMIT
}
```

This prevents an assignment that resembles forbidden global mutation from silently creating a local and keeps every global reference stable under nearby edits. The check reuses ordinary visible-symbol lookup and adds no new scope mechanism.
