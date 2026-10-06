# Type annotations do not create borrows
status: amended by [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md)

Assignment preserves the ownership category of the value it binds. Assigning an owner moves it. Assigning a shared or exclusive borrow binds that borrow. A type annotation checks or narrows the value's type but does not turn an owner into a borrow:

```casa
0 items.get = item: $Item
```

Calls and constructors auto-borrow according to their declared parameters. Field and collection observation and closure capture produce borrows through their existing operations. Those sources cover current uses without a separate owner-to-local-borrow operation. Runtime state belongs to the root body under ADR-0165.

Every owned or borrowed local binding remains reassignable. `$T` prevents mutation of the borrowed value, while `mut$T` permits mutation through an exclusive loan. Neither qualifier controls binding mutability. Casa adds no `mut` binding declaration, annotation-triggered loan, `borrow` expression, `ref` keyword, or unary address-of operator. A direct local-borrow operation remains deferred until real code cannot compose naturally through existing borrow-producing operations.
