# Custom destruction uses a reserved drop method

A type defines custom cleanup with the reserved inherent method `fn drop mut$self`. This is a compiler lifecycle hook rather than a trait implementation. Casa does not define a public `Drop` trait, a derivation, or a generic destructor bound.

The compiler accepts only the exact mutable-borrow receiver and no-output stack effect. Source code cannot call the method directly; the `drop` intrinsic consumes an owner and the compiler invokes the hook automatically during the same destruction lowering used at scope exits and early returns.

## Considered options

- A compiler-known `Drop` trait fits explicit implementation syntax, but remains special in invocation and ownership while offering little useful generic abstraction.
- Dedicated destructor grammar makes the lifecycle role explicit, but adds syntax for behavior already identified by one reserved method.
- Requiring explicit `close` calls avoids a compiler hook, but permits resource leaks on ordinary control-flow exits.
- A reserved method is local, explicit, and reuses existing method declarations without pretending cleanup is an ordinary callable capability.

## Consequences

- Defining the reserved `drop` method makes a type ineligible for `Copy`.
- Custom cleanup runs first through an exclusive borrow; fields are then destroyed automatically in reverse declaration order.
- The cleanup method cannot move fields out through its borrowed receiver and cannot be called directly or referenced as a function value.
- The `drop` intrinsic remains the sole explicit source operation for destroying an owner.
- Destruction still happens exactly once. A panic during cleanup terminates because Casa does not unwind.
- Receiver spelling is a separate surface-syntax decision; changing it does not alter this lifecycle contract.
