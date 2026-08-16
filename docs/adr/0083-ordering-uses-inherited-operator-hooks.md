# Ordering uses inherited operator hooks

PartialOrd owns the four boolean ordering hooks used directly by operators and the partial three-way comparison primitive. Ord refines it with total equality and total three-way comparison:

```casa
trait PartialOrd: PartialEq {
    fn partial_cmp $self other:$self -> Option[Ordering]
    fn lt $self other:$self -> bool { ... }
    fn le $self other:$self -> bool { ... }
    fn gt $self other:$self -> bool { ... }
    fn ge $self other:$self -> bool { ... }
}

trait Ord: PartialOrd + Eq {
    fn cmp $self other:$self -> Ordering

    fn partial_cmp $self other:$self -> Option[Ordering] {
        other self.cmp Option::Some
    }
}
```

The standard boolean defaults interpret `partial_cmp`; Ord fills that inherited requirement by wrapping `cmp`. Implementations may override the boolean hooks while preserving the same ordering semantics. `<`, `<=`, `>`, and `>=` lower to `lt`, `le`, `gt`, and `ge` respectively. The compiler validates effective inherited shape, so Ord exposes all four hooks plus `partial_cmp` and `cmp` even though most are inherited or defaulted.

## Consequences

- IEEE floats implement PartialEq and PartialOrd but not Eq or Ord.
- `derives Ord` generates the total equality and comparison primitives and declares PartialEq, Eq, PartialOrd, and Ord conformances; standard defaults provide the partial adapter and boolean hooks.
- A manual total implementation may place all four conformances in one impl block; operator hooks normally use their defaults.
- Overriding a boolean hook may improve performance but must agree with `partial_cmp` or `cmp`; the compiler cannot prove this semantic law.
- Current direct operator-to-method lowering is retained while total-order algorithms may use `cmp` to avoid repeated comparisons.
