# Raw input is bytes

File contents, standard input, captured process output, process arguments, environment values, directory entry names, and other external data without a text guarantee enter safe Casa code as owned `Bytes`. `Bytes.to_str $self -> Result[str Utf8Error]` explicitly validates UTF-8 and creates owned text. OS failures remain `IoError`; invalid text is not disguised as an OS error.

The initial file API exposes `file::read_all path:$str -> Result[Bytes IoError]`. Equivalent raw standard-input and process-capture APIs return `Bytes`. Safe text consumers validate at the point where they possess the domain knowledge that the input is intended to be text.

## Considered options

- Returning `str` from every input API keeps current call sites short, but allows invalid external bytes to inhabit Casa's validated text type.
- Replacing invalid sequences during input is convenient for display, but silently changes source files, protocols, and binary data.
- Returning a combined text-read error from every raw API mixes byte transport with an optional interpretation.
- Returning `Bytes` preserves the input exactly and makes the text boundary explicit.

## Consequences

- Syscall and foreign wrappers may fill byte storage inside `unsafe`, but publish only the initialized length through safe `Bytes` operations.
- Binary consumers do not pay for UTF-8 validation or replacement.
- Text consumers handle `Utf8Error` explicitly or map it into their own domain error before using `?`. `to_str` preserves the source by validating and copying.
- Casa does not initially add a general `read_text` convenience function. Repeated callers may justify one later together with an explicit combined error type; invalid UTF-8 is never folded into `IoError`.
- Byte-oriented line reading may delimit on byte `0x0A`; a text-line convenience operation must validate before returning `str`.
- Allocation failure follows Casa's process-termination policy.
