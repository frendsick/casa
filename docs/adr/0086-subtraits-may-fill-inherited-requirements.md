# Subtraits may fill inherited requirements

A subtrait may provide a default body for a bodyless method required by a supertrait when the method name and stack effect match exactly. The inherited requirement then has an implementation for types that satisfy the subtrait:

```casa
trait PartialOrd: PartialEq {
    fn partial_cmp $self other:$self -> Option[Ordering]
}

trait Ord: PartialOrd + Eq {
    fn cmp $self other:$self -> Ordering

    fn partial_cmp $self other:$self -> Option[Ordering] {
        other self.cmp Option::Some
    }
}
```

A type implementing only PartialOrd must still supply `partial_cmp`. A type implementing Ord may use Ord's default. This is ordinary default-method inheritance: Option construction occurs in standard-library code and receives no compiler-specific treatment.

## Consequences

- `derives Ord` needs to generate total comparison primitives while standard defaults provide the partial adapter and boolean operator methods.
- A subtrait cannot change an inherited method's stack effect.
- One matching body may satisfy identical inherited requirements; incompatible same-name requirements remain a declaration error.
- An inherent method still overrides a default under the existing trait implementation rule.
