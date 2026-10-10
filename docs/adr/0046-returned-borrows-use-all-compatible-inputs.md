# Returned borrows use all compatible inputs

status: amended by [ADR-0175](0175-returned-borrow-sources-are-inferred-from-checked-bodies.md)

General, explicitly written `fn[...]` contracts and calls checked only against
trait requirements tie returned borrows to every compatible input. Shared
results can use shared or exclusive inputs. Exclusive results require
exclusive sources. Borrowed payloads carried by owned inputs and callable
capture dependencies also remain live when the result can depend on them.

Passing a callable through an explicit function type deliberately erases its
inferred source precision. A known target or optimization cannot recover it.
Ordinary named calls and function values with inferred types use the checked
summaries defined in ADR-0175.

## Consequences

- Abstract contracts need no source annotations or named lifetime parameters.
- Each possible source must remain alive until the result's last use.
- Returning a borrow derived from a local owner remains a compile-time error.
- The complete-owner rule in [ADR-0108](0108-opaque-returned-borrows-keep-the-complete-input-loaned.md) still applies.
