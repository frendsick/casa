# Clone derivation requires Clone bounds

`derives Clone` requires every owned field or variant payload to implement Clone. It does not introduce a disjunctive “Clone or Copy” constraint and does not special-case Copy fields during generic constraint solving.

Under Casa's standard `trait Copy: Clone { }` declaration, every standard Copy type already satisfies that requirement. A generic declaration such as `struct Box[T] derives Clone` therefore has the single conditional bound `T: Clone`, which naturally accepts Copy types through ordinary supertrait satisfaction.

A freestanding environment that declares Copy without extending Clone does not gain this relationship; its Copy-only types cannot participate in Clone derivation until they also implement Clone.

## Consequences

- Clone derivation exposes one ordinary, expressible bound rather than an unsupported disjunction.
- User-defined standard Copy types need only declare Copy; missing fieldwise Clone behavior is supplied as part of satisfying Copy's declared supertrait.
- A non-Copy type remains clonable whenever it implements Clone.
