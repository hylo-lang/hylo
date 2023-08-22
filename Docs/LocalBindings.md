# Local bindings

A binding declaration at the source level introduces zero or more bindings that are treated individually after their declaration.
For example:

```hylo
public fun foo() -> Int {
  let (one, two): { Int, Int }
  one = 1
  two = 2
  print(one)
  return two
}
```

Although `one` and `two` are introduced by the same declaration, they are neither initialized nor deinitialized at the same time.
To accurately keep track of local bindings individually, initialization, lifetime, and ownership analyses are done at a lower level of abstraction: Hylo's intermediate representation.

## Local storage

A local binding is said to have storage if the value that it represents may be stored at a memory location allocated for that binding.

The lifetime of a binding's storage is bound to the lexical scope of that binding.
At the IR level, that lifetime is equal to the live-range of the `alloc_stack` representing the binding's storage.

*Note: The live-range of binding's storage is never empty in a well-formed program because an `alloc_stack` must be operand of exactly one `dealloc_stack` on each possible execution path.*

### `var` bindings

A `var` binding always have storage.

```hylo
public fun main() {
  var foo = 42
}
```

`foo` has storage because it is initialized with a rvalue.
In raw IR, `main` is translated as follows:

```
@lowered fun main() -> Void {
bb0:
  %0 = record Hylo.Int, i64(0x2a)
  %1 = alloc_stack Hylo.Int, binding="foo"
  %2 = start_borrow [set] %0
  store %0, %2
  dealloc_stack %0
}
```

The first instruction corresponds to the evaluation of the integer literal, resulting in a rvalue assigned to `%0`.
The next instruction allocates storage for `foo`.
The address of that storage is then borrowed to initialize it with the store instruction.

### `let` bindings

At the source level, the initializer of a binding is the expression of an object that initializes that binding.

*Note: a binding might be initialized with a value that is only a part of a larger object evaluated by its initializer (e.g., `let (a, _) = make_pair()`).*

A `let` binding may several possible initializers if its initialization depends on control flow.
For example, `bar` has two initializers in the following snippet: one in each branch of the conditional expression.

```hylo
public fun main() {
  let bar = if condition { 1 } else { 2 }
}
```

For a `let` binding of type `T`:
1. no storage is allocated if all its initializers evaluate to lvalues;
2. storage of type `T` is allocated if all its initializers evaluate to rvalues;
3. storage of type `Optional<T>` is allocated if one of its initializers evaluates to a rvalue and another to a lvalue.

*Note: the first case is identical to the behavior of a `var` binding.*

In the second case, storage is allocated with a `nil` value on every execution path where the corresponding `let` binding is initialized with a lvalue.
For example:

```hylo
public fun main() {
  var foo = 42
  let bar = if condition { foo } else { 42 }
  _ = bar.copy()
}
```

In raw IR, `main` is translated as follows:

```
@lowered fun main() -> Void {
bb0:
  %0 = record Hylo.Int, i64(0x2a)
  %1 = alloc_stack Hylo.Int, binding="foo"
  %2 = start_borrow [set] %0
  store %2, %0
  %3 = alloc_stack Hylo.Option<Hylo.Int>
  %4 = start_borrow [let] @condition, 0
  %5 = call [let, let] @Builtin.i1_copy, %4
  cond_branch %5, bb1, bb2
bb1:
  %6 = record Hylo.Optional<Hylo.Int>.Nil
  %7 = union Hylo.Option<Hylo.Int>, %6
  %8 = start_borrow [set] %3
  store %7, %8
  %9 = start_borrow [let] %0, binding="bar"
  branch bb3, %9
bb2:
  %10 = record Hylo.Int, i64(0x2a)
  %11 = record Hylo.Optional<Hylo.Int>.Some, %10
  %12 = union Hylo.Option<Hylo.Int>, %11
  %13 = start_borrow [set] %3
  store %12, %13
  %14 = start_borrow [let] %3, 1, binding="bar"
  branch bb3, %14
bb3(%15 : &Int):
  %16 = start_borrow [let] %15
  %17 = call [let, let] Hylo.Int.copy, %16
  deinit %17
  dealloc_stack %3
  dealloc_stack %0
}
```

The initialization of `bar` starts with the `alloc_stack` assigned to `%3`, representing the allocation of its storage.
The program jumps to either `bb1` or `bb2` depending on the value of `condition`.
In the first branch, `bar`'s storage is initialized with a nil value.
Then, a `let` access to `foo`'s storage is borrowed and passed as argument to the block `bb3` before jumping to it.
In the second branch, `bar`'s storage is initialized with the result of an integer literal wrapped inside a optional container.
Then, that value is borrowed and passed as an argument to `bb3` before jumping to it.

Crucially, `bar`'s storage gets allocated in both branches.
Hence, it will be safe to deinitialize that storage in `bb3` with `Optional`'s deinitializer, regardless of the execution path that will have been taken at runtime.

### `inout` bindings

An `inout` binding never have storage and is always represented as a borrowed access at the IR level.
For example:

```hylo
public fun main() {
  var foo = 42
  inout bar = &foo
}
```

In raw IR, `main` is translated as follows:

```
@lowered fun main() -> Void {
bb0:
  %0 = record Hylo.Int, i64(0x2a)
  %1 = alloc_stack Hylo.Int, binding="foo"
  %2 = start_borrow [set] %0
  store %0, %2
  %3 = start_borrow [inout] %0, binding="bar"
  dealloc_stack %0
}
```

## Initialization states

The initialization state of the objects in the scope of a function is tracked to fend against undefined behavior.
The tracking is done at the IR level, during __definite initialization__.

Objects are said to live in registers if they are assigned to a local register.
Otherwise, they are said to live in memory.
For example:

```
@lowered fun add(let Int, let Int) -> sink Int {
bb0(%0 : &Int, %1 : &Int):
  %2 = start_borrow [let] %0
  %3 = start_borrow [let] %1
  %4 = call [let, let, let] @Hylo.Int.infix+, %2, %3
  return %4
```

`add` has two `let` parameters accepting integer arguments.
In the scope of the function, those arguments live in memory at the locations denoted by `%0` and `%1`.
Meanwhile, the result of the call to `Hylo.Int.infix+(_:_:)` lives in register and is assigned to `%4`.

Objects that live in register are *initialized* when first assigned to a register and get eventually *consumed* before the function returns.
Because they are treated as linear resources, any operation in which an object appears as an operand consumes it.
For example, the `return` instruction in the program above consumes the object living in `%4`.

The state of an object living in memory is tracked via the initialization state of its storage.
A memory location may be in the following states:
- *Raw*: location is allocated but not bound to any type.
- *Uninitialized*: location is bound to a type but providing storage for any object.
- *Initialized*: memory is providing storage for a fully initialized object.
- *Partial*: memory is providing storage for an object that is only partially initialized or consumed.

*Note: `alloc_stack` allocates a memory and binds it to a type at the same time.*
*Hence, the resulting location is uninitialized rather than raw.*

Instructions that operate on memory locations typically have preconditions and postconditions on the initialization state of those locations.
For example, a `load` instruction requires its source location to be initialized and leaves it uninitialized.

## Deinitialization

Local storage is deinitialized when execution reaches the end of its lifetime.
The compiler guarantees this property and inserts `deinit` instructions as necessary during __definite initialization__ (DI).

DI is implemented as an intraprocedural abstract interpreter that models the contents of the call stack and local registers.
The abstract interpreter keeps track of the initialization state of the memory and maps local registers to the set of memory locations they may represent.

Before evaluating each instruction, the interpreter verifies that its operands have a suitable initialization state.
If they don't, it either inserts additional instruction to satisfy the constraints or concludes that the program is ill-formed and emits a diagnostic.
