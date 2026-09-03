# Imported modules do not run top-level bodies
status: amended by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

A module's top-level program body runs only when that module is the root program. Importing a module contributes its declarations and runs the exactly-once immutable-global initializers in its dependency closure. The imported top-level statements that are not global initializers, such as unrelated I/O, `exit`, and test execution, do not run.

Global initializers are ordinary Casa code and may call ordinary functions, so an import can produce the effects those initializers perform. Casa imposes no effect restriction on initializers and runs no effect analysis. An initializer that needs I/O, a clock, or an environment lookup is allowed to use it, and the effect happens once at initialization in the order ADR-0053 defines.

This makes full and selective imports consistent and keeps dependencies free of hidden top-level program behavior. Casa retains convenient root-level scripting. Requiring a distinguished `main` entry point remains a separate decision.

## Consequences

- A module intended as a library keeps its initializers cheap and effect-free by convention, not by a compiler rule.
- Restricting initializer effects stays available as a later decision, because it needs either a new effect restriction or effect analysis that Casa does not have.
