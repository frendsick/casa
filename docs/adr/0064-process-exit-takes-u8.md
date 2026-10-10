# Process exit takes u8

The safe OS-library operation `std::process::exit status:u8` terminates immediately without unwinding or cleanup. Normal root completion destroys root owners and exits with status zero.

```casa
0 std::process::exit
1 std::process::exit
```

Using `u8` matches the status range conventionally observable by a parent process and contextually types ordinary status literals without a conversion. The wrapper contains the unsafe non-returning platform syscall. Process exit is not a second compiler intrinsic.
