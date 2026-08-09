# Module import cycles are rejected

Any cycle in the module import graph is a compile-time error, including cycles that contain only declarations and no global initialization. The diagnostic reports the complete import chain that closes the cycle.

Programs break a cycle by moving shared declarations into a third module imported by both participants. Rejecting cycles keeps module loading and namespace initialization single-pass and deterministic; Casa does not build partially initialized namespaces or a multi-module declaration fixed point.
