# Unsafe functions are not first-class

Casa initially rejects taking a function reference to an `unsafe fn`. Unsafe functions may be called directly only from an unsafe context; they cannot be converted to ordinary `fn[...]` values because that would erase the caller's safety obligation.

A safe function or repeatable closure may contain a narrowly justified `unsafe` block and remains first-class because its implementation assumes responsibility for preserving a safe calling contract. This is the ordinary way to expose a reusable safe wrapper.

Casa adds no `unsafe fn[...]` function-value type until a concrete callback or foreign-interface use requires first-class unsafe calls. This avoids another callable family and prevents indirect `exec` from bypassing the unsafe boundary.
