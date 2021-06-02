#  Val's Type System

## Casting

There are three built-in casting operators in val:
1. The upcast operator `as`
2. The downcast operator `as?`
3. The unsafe cast operator `as!`

A casting operator accepts a value expression of type `T` with a type expression denoting a type `U`.
The operator serves to modify the value on the left so that it is assumed to have type `U`  (or `Maybe<U>`).

```val
view Bar {}
type Foo: Bar {}

val foo = Foo()
val bar = foo as? Bar
```

At last line of the above example, `foo` is an identifier denoting a value binding and `Bar` is a type signature.


### Upcast operator

The upcast operator applies when the type of the left operand is statically known to be a subtype of the type expressed by the right operand.
As a result, it is always a safe operation that can never fail.

### Downcast operator

The downcast operator applies when the type of the left operand is potentionally a subtye of the type expressed by the right operand.
This situation typically occurs when attempting to "downcast" an expression to a type more specific.

Because the operation might be incorrect (e.g., if the value being cast is not actually a subtype expressed by the right operand), the type of a downcast to type `U` is always `Maybe<U>`.

### Unsafe cast operator

The unsafe cast operator forces the type of the left operand to be cast as the type expressed by the right operand.
The operation fails with a runtime error if the conversion was actually illegal.
