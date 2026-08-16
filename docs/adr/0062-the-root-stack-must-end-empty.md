# The root stack must end empty

The executable root body must have stack effect `None -> None`. Root locals are destroyed normally at completion, but values left unconsumed on the value stack are compile-time errors rather than implicit outputs or implicit drops.

```casa
42 # error: root leaves i64 on the stack
```

The root has no caller to receive outputs, and silently discarding them can hide a missing call, print, assignment, or explicit `drop`. Enforcement is one final stack-effect check and adds no runtime work.
