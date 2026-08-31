# Borrow errors show contracts and related loan locations
related issue: #611

Casa keeps `T`, `$T`, and `mut$T` as the owned, shared, and exclusive type
forms. Calls continue to reborrow values implicitly from the callee contract.
Call sites do not add borrow markers or access-mode words.

When a call fails because the available access cannot meet a parameter or
receiver contract, the primary error shows the required and available type
forms. It names the operation, parameter, or receiver when that context is
known. It uses source-facing type names and suppresses a generic follow-on
type mismatch for the same conflict.

When an active loan causes the conflict, related source locations show where
the loan started and, when known, the later use that keeps it active. The
compiler suggests cloning only when the value implements `Clone`. Overlapping
exclusive arguments keep their direct conflict wording and name the affected
parameters when possible.

A full ownership trace is deferred. Compact contract details and related
locations cover the observed failures without a new diagnostic framework or
new borrow syntax.

## Consequences

- Borrow diagnostics describe the capability mismatch before suggesting a
  repair.
- One rejected operation produces one primary diagnostic, with related
  locations as notes.
- The existing `CasaError.expected`, `CasaError.got`, and located `CasaNote`
  values remain the diagnostic model.
