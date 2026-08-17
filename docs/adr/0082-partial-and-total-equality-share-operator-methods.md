# Partial and total equality share operator methods

Partial and total equality use the same `eq` and `ne` operator methods. The standard declarations place them on PartialEq and make Eq the explicit lawful-total refinement:

```casa
trait PartialEq {
    fn eq $self other:$self -> bool

    fn ne $self other:$self -> bool {
        other self.eq !
    }
}

trait Eq: PartialEq { }
```

The compiler validates effective trait shape after collecting inherited methods. A reserved Eq declaration must therefore expose correctly typed `eq` and `ne` operator methods either directly or through supertraits; an empty standalone `trait Eq { }` is invalid as the language Eq contract.

Explicit implementation distinguishes semantic strength, so separate method names such as `partial_eq` are unnecessary. A float may implement PartialEq without adopting Eq, while a lawful total value explicitly implements both. Equality operators accept PartialEq and lower to `eq` or `ne`; Hashable requires Eq.

## Consequences

- `Eq` promises that the inherited operations form lawful total equality but introduces no additional method name.
- `derives Eq` generates PartialEq behavior and implements both PartialEq and Eq.
- A handwritten total implementation may use one block: `impl Point: PartialEq + Eq { fn eq ... }`; `ne` normally comes from its default.
- Primitive equality remains intrinsic when no traits are present and satisfies the active contracts when they are declared.
