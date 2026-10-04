# Integer arithmetic is checked consistently

Ordinary integer `+`, `-`, `*`, and negation detect overflow and panic in every build mode. Integer literals outside their contextual or defaulted type are compile errors. Recoverable arithmetic uses `try_add`, `try_sub`, and `try_mul`, returning `Option[T]`. Intentional modular arithmetic uses `wrapping_add`, `wrapping_sub`, and `wrapping_mul`. Saturating arithmetic remains deferred until a concrete use requires it.

Division and remainder truncate toward zero, with the remainder carrying the dividend's sign. `/` and `%` panic on division by zero and on signed minimum divided by `-1`. `try_div` and `try_mod` return `Option[T]` for recoverable use. Shift counts use `u64` and must be smaller than the operand width. Left shift discards shifted-out bits, signed right shift preserves the sign, and unsigned right shift inserts zero bits. Recoverable shift operations remain deferred.

Checked operators prevent silent corruption and keep behavior independent of
build mode. Requiring `Option` for ordinary arithmetic would burden composition.
The x86-64 overflow branch uses non-unwinding panic. Proven-safe check elimination
may be added later.
