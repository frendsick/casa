# Raw-pointer borrows use existing input origins

Because `ptr` carries no lifetime, a typed borrow formed with `ptr::as_ref[T]` or `ptr::as_mut[T]` may escape a function only when it is anchored to an existing borrowed input. The result conservatively carries every borrowed input origin that could keep the raw storage valid:

```casa
fn first self:$Buffer -> $u8 {
    unsafe {
        self.data ptr::as_ref[u8]
    }
}
```

The unsafe implementation promises that the pointer remains valid for the inferred duration of `self`. A borrowed return with no input origin is rejected:

```casa
unsafe fn arbitrary address:ptr -> $u8 { # error
    unsafe { address ptr::as_ref[u8] }
}
```

## Consequences

- Raw conversion does not invent a `'static` or caller-chosen lifetime.
- Safe collection and owner methods may expose borrowed storage through their borrowed receiver without additional anchor syntax.
- Unsafe code is responsible for proving that every selected input origin really outlives and owns or otherwise stabilizes the pointed-to storage.
- A function with no suitable borrowed input returns `ptr` or copies into an owned value instead of returning a borrow.
- A raw borrow used only locally may remain within the current checked scope, subject to the ordinary unsafe validity and aliasing obligations.
