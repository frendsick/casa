# `panic` is a minimal terminating intrinsic

`panic` is a compiler-known operation with input `$str`. It writes the borrowed UTF-8 message to standard error and terminates with a fixed nonzero status without unwinding or running cleanup.

```casa
"invalid parser state" panic
```

The primitive performs no allocation, formatting, backtrace capture, payload construction, or catching. Callers may format a message before invoking it when allocation remains safe; compiler-generated checks and allocation-failure paths use static messages. Its non-returning behavior participates in the internal control-flow fact defined by ADR-0061.
