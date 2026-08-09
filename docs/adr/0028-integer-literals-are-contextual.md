# Integer literals are contextual

An integer literal retains its sign and exact magnitude through local type inference. An immediate expected type may resolve it to any Casa integer type; the compiler then verifies that the value is representable. A literal still unconstrained after local inference defaults to `i64`. Casa initially adds no integer suffix syntax.

Examples include `255 data.push`, where a `u8` parameter constrains `255`; `1 = count`, which defaults to `i64`; and `1 = count:u8`, where the binding annotation constrains the literal. A literal used with an already typed integer operand adopts that operand's type. This contextual typing applies only to literals and does not implicitly promote or convert stored integer values.

## Considered options

- Typing every literal as `i64` during parsing is simple, but makes ordinary calls such as `255 data.push` require explicit conversion despite the unambiguous `u8` parameter.
- Mandatory suffixes make widths locally visible, but add noise where a parameter, binding, field, or return type already supplies the answer.
- Full expression-wide numeric coercion is convenient, but weakens the explicit-width and no-promotion rules.
- Local contextual typing gives literals ergonomic flexibility without adding conversions between typed values.

## Consequences

- Literals are range-checked after their type is resolved. `256 = value:u8` and `-1 = value:u8` are compile errors, while `-128 = value:i8` is valid.
- The compiler may represent an unresolved literal as a sign plus checked `u64` magnitude because no supported integer type exceeds `u64`; a source magnitude beyond that range is an error. Arbitrary-precision arithmetic is unnecessary.
- Arithmetic involving a typed operand constrains compatible literals to that operand's type. Two otherwise unconstrained literal operands default to `i64`.
- Generic parameters, constructor fields, function parameters, annotated bindings, and declared returns may provide the expected type through existing local constraints.
- Diagnostics report the literal value, resolved target type, and permitted range.
