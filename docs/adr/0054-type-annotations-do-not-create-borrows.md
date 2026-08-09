# Type annotations do not create borrows

Assignment preserves the ownership category of the value it binds. Assigning an owner moves it; assigning a shared or exclusive borrow binds that borrow. A type annotation checks or narrows the value's type but does not turn an owner into a borrow:

```casa
items.get.unwrap = item:$Item
```

Calls and constructors auto-borrow according to their declared parameters. Field and collection observation, immutable globals, and closure capture produce borrows through their existing operations. Those sources cover current uses without a separate owner-to-local-borrow operation.

Every owned or borrowed local binding remains reassignable. `$T` prevents mutation of its referent through that value, while `mut$T` permits mutation through an exclusive loan; neither qualifier controls binding mutability. Casa adds no `mut` binding declaration, annotation-triggered loan, `borrow` expression, `ref` keyword, or unary address-of operator. A direct local-borrow operation remains deferred until real code cannot compose naturally through existing borrow-producing operations.
