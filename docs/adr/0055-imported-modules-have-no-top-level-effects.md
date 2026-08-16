# Imported modules have no top-level effects

Arbitrary top-level operations execute only in the root program. Imported modules contribute declarations and run their exactly-once immutable-global initializers, but importing a module cannot perform unrelated I/O, call `exit`, run tests, or otherwise execute its top-level program body.

This makes full and selective imports consistent and keeps dependencies free of hidden behavioral effects. Casa retains convenient root-level scripting; requiring a distinguished `main` entry point remains a separate decision.
