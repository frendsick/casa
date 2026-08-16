# Floating-point literals are contextual

A floating-point literal is typed from immediate context as either `f32` or `f64`. When no context constrains it, the literal defaults to `f64`. Casa initially adds no floating-point suffix syntax.

The compiler converts the source decimal directly to the selected IEEE width using round-to-nearest, ties-to-even. It does not first round through `f64` when the target is `f32`.

## Consequences

- Parameters, annotated bindings, aggregate fields, and generic arguments can provide the literal's expected width.
- A finite source literal that overflows the selected width is a compile-time error rather than silently becoming infinity.
- Values too small for the selected width round to a subnormal or zero according to IEEE rules.
- Runtime arithmetic may still produce infinity or NaN under ADR-0012.
- Already typed floating-point values never change width implicitly.
- Literal spelling follows the minimal decimal grammar in ADR-0146.
