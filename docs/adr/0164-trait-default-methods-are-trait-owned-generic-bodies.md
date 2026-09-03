# Trait default methods are trait-owned generic bodies
related issue: #659

Casa retains direct and inherited trait default methods. They let a trait define
one required primitive and express consistent adapters or convenience behavior
once. Removing defaults would duplicate that behavior across implementations
and move standard-library policy into the compiler.

A bodyless requirement needs an implementation unless one compatible inherited
default supplies it. Defaults are inherited transitively, a shared declaration
in a diamond remains one declaration, and a subtrait may provide a matching body
for an inherited requirement. Distinct applicable defaults remain ambiguous.
An explicit method in the trait implementation may replace a default when its
stack effect matches exactly. It cannot merge into a derived implementation
under [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md).
An inherent method can win ordinary lookup, but it does not satisfy a bodyless
trait requirement.

Each default body belongs to its declaring trait. The compiler checks it once
under abstract `self`, trait and method parameters, and declared bounds. An
unqualified call to another method in that trait resolves in the same
instantiated trait implementation. Calls to another trait use qualification.
Concrete implementations use the ordinary generic specialization path. The
compiler does not create synthetic source functions or recheck cloned bodies for
each receiver.

Trait resolution rejects inheritance cycles, incompatible inherited stack
effects, and invalid default bodies. Implementation resolution rejects missing
requirements and mismatched overrides. Call resolution reports distinct
applicable defaults and names each candidate trait. An instantiation error keeps
the default body's source location and identifies the concrete implementation
context.

## Consequences

- Standard traits keep their minimum required primitives and source-defined
  defaults for equality, ordering, iteration, and other adapters.
- Valid source needs no migration. Tests move from generated names and cloned
  declarations to direct, inherited, generic, overridden, qualified, diamond,
  and ambiguous behavior through production compiler entry points.
