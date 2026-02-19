# Casa

**casa** is a statically typed, stack-based programming language with functional programming influences.

- **Pipeline**: source → lex → parse → type check → bytecode → emit asm → compile asm → run

## Key files

| File | Purpose |
|------|---------|
| `casa/lexer.py` | Tokenizer |
| `casa/parser.py` | Op tree builder, identifier resolution |
| `casa/typechecker.py` | Stack-based type inference and checking |
| `casa/bytecode.py` | Op → Inst lowering |
| `casa/emitter.py` | GNU assembly emitter |
| `casa/compiler.py` | Compiles GNU assembly code to x86_64 executable |
| `casa/common.py` | All shared types: `OpKind`, `InstKind`, `Keyword`, `Intrinsic`, `Operator`, `Signature`, `Function`, `Struct`, … |
| `lib/std.casa` | Standard library (`memcpy`, `array`, `List`) |
| `examples/` | Example programs |
| `.claude/commands/` | Slash command agents: `designer`, `documenter`, `programmer`, `reviewer`, `tester` |

## Language features

### Types
| Type | Description |
|------|-------------|
| `int` | Integer |
| `bool` | Boolean (`true` / `false`) |
| `str` | String |
| `ptr` | Heap pointer (from `alloc`) |
| `array` | Fixed-size array literal |
| `fn[sig]` | Function/lambda type, e.g. `fn[int int -> int]` |
| `any` | Escape hatch, matches anything |
| `SomeStruct` | User-defined struct type |

### Literals
```
42          # int
true false  # bool
"hello"     # str
[1, 2, 3]   # array (literal items only)
```

### Operators
- Arithmetic: `+` `-` `*` `/` `%`
- Bitshift: `<<` `>>`
- Comparison: `==` `!=` `<` `<=` `>` `>=`
- Boolean: `&&` `||` `!`
- Assignment: `= name`  `+= name`  `-= name`

### Stack intrinsics
```
drop   # ( a -- )        discard top
dup    # ( a -- a a )    duplicate top
swap   # ( a b -- b a )  swap top two
over   # ( a b -- a b a) copy second to top
rot    # ( a b c -- b c a ) rotate top three
```

### Memory intrinsics
```
alloc  # ( int -- ptr )  heap-allocate N slots
load   # ( ptr -- any )  read heap at address
store  # ( any ptr -- )  write heap at address
```

### IO
```
print  # ( any -- )  print top of stack
```

### Functions
```
fn name param:type -> return_type {
    body
}
```
- Parameters popped from stack in declaration order (first param = top)
- Return types describe what remains on the stack after the call
- Signatures can be explicit or inferred by the type checker
- Must be defined at global scope

### Variables
```
42 = count    # global or local assignment
1 += count    # increment
1 -= count    # decrement
```
- Global: declared at top level, visible everywhere
- Local: declared inside a function, scoped to that function

### Control flow
```
if condition then
    ...
elif other_condition then
    ...
else
    ...
fi

while condition do
    ...
    break
    continue
done
```
The type checker enforces stack consistency across all branches and after loops.

### Structs
```
struct Person {
    name: str
    age:  int
}
```
- Instantiation: push fields bottom-to-top, then call struct name: `18 "John" Person`
- Auto-generated getter `Type::field` and setter `Type::set_field`
- Dot / arrow shorthand: `person.name` → `person Person::name`, `"Jane" person->name` → `"Jane" person Person::set_name`

### impl blocks
```
impl TypeName {
    fn method self:TypeName -> RetType { body }
}
```
- Namespaced as `TypeName::method`
- Dot sugar: `instance.method` → `instance TypeName::method`
- Can span multiple `impl` blocks and files

### Lambdas / closures
```
{ body }   # ( -- fn[sig] )  create lambda, captures enclosing variables
exec       # ( fn[sig] -- )  call lambda on top of stack
```
Example: `{ 2 * }` has type `fn[int -> int]`

### Type cast
```
(TypeName)   # pops top, pushes as TypeName (no runtime check)
```

### Include
```
include "relative/path/to/file.casa"   # included at most once
```

## Standard library (`lib/std.casa`)
- `memcpy dst:ptr src:ptr n:int` — copy n heap slots
- `array::length array -> int` — length of array (stored in first slot)
- `array::nth array:array n:int -> any` — nth element
- `List` struct — dynamic list with `from_array`, `nth`, `push`

## Architecture notes

### Type checker (`typechecker.py`)
- Simulates the stack symbolically with types instead of values
- Infers function signatures by replaying ops
- Compares inferred signature against declared one when both exist
- `BranchedStack` tracks stack state before/after conditionals and loops
- `ANY_TYPE = "any"` acts as a wildcard that matches any type

### Parser / identifier resolution (`parser.py`)
- `resolve_identifiers` converts `IDENTIFIER` ops to `FN_CALL`, `PUSH_VARIABLE`, `STRUCT_NEW`, etc.
- Lambda functions are registered globally as `lambda__<scope>_o<offset>`
- Method calls (`.foo` / `->foo`) are resolved to `FN_CALL` during type checking using the receiver type

### Bytecode (`bytecode.py`)
- Ops lower to `Inst` objects with `InstKind`
- VM has: `data_stack`, `call_stack` (doubles as locals frame), `globals`, `heap`, `strings`
- `CONSTANT_LOAD/STORE` used for string interning

### Assembly emitter (`emitter.py`)
- Target: x86-64 Linux (System V ABI), uses hardware stack as data stack

### Compilation
- Link assembly code with `ld`
- Compile with `as`
- The result is a statically linked x86_64 executable

## Documentation

When any language feature, intrinsic, operator, or standard library function is changed or added, the corresponding documentation in `docs/` and `README.md` must be updated to reflect the change.
