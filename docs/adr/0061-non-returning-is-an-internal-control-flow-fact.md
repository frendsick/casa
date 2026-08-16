# Non-returning is an internal control-flow fact

Casa represents termination as compiler control-flow metadata rather than a source `Never` type or `noreturn` annotation. `panic`, `exit`, and direct calls whose implementations are proven not to return end their path; that path does not participate in subsequent stack or ownership joins.

```casa
fn require_value[T] item:Option[T] -> T {
    item match
        Option::Some(value) => value
        Option::None => "missing value" panic
    end
}
```

Non-returning status may propagate through direct named calls when all reachable paths terminate. It is not part of `fn[...]`: indirect calls are conservatively assumed to return, so first-class function types need no additional family or effect syntax. The analysis is a small call-graph/control-flow fact and remains covered by the compiler self-compilation benchmark.
