# Float arithmetic is same-width IEEE arithmetic

The `+`, `-`, `*`, and `/` operators accept two values of the same floating-point width and produce that width. Casa performs no implicit `f32`/`f64` promotion.

## Consequences

- Floating-point division by zero follows IEEE behavior and may produce infinity or NaN rather than terminating.
- Operations round their result to the operand width.
- Mixed-width arithmetic is a type error; callers use an explicit conversion first.
- `%` remains integer-only initially. Named floating-point remainder operations may be added when a concrete need establishes the desired semantics.
- Checked arithmetic from ADR-0018 applies to integers, not floats.
