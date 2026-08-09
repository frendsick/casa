# Diamond inheritance deduplicates shared declarations

When one trait declaration is inherited through multiple paths, it remains one declaration. Diamond inheritance therefore does not create an ambiguity by itself:

```casa
trait Base {
    fn show $self -> str { ... }
}

trait Left: Base { }
trait Right: Base { }
trait Both: Left + Right { }
```

`Both` inherits one `Base.show`, identified by its original declaration. Two distinct inherited declarations with the same name and compatible stack effects remain competing defaults under the ordinary ambiguity rule.

## Consequences

- Inherited method collection deduplicates by declaration identity, not by method name or inheritance path.
- A shared ancestor's requirements and defaults are checked once.
- Distinct competing defaults require an inherent implementation or a default supplied by the child trait.
- Casa needs no parent-qualification syntax merely to support diamonds.
