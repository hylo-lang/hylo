#  Val's Type System

## Casting

There are two built-in casting operators in val:
1. The static cast operator `as`
3. The dynamic cast operator `as!`

A casting operator accepts a value expression of type `T` with a type expression denoting a type `U`.
The operator serves to modify the value on the left so that it is assumed to have type `U`.

```val
view Bar {}
type Foo: Bar {}

val foo = Foo()
val bar = foo as! Bar
```

At last line of the above example, `foo` is an identifier denoting a value binding and `Bar` is a type signature.


### Static cast operator

The static cast operator applies when the type of the left operand is statically known to be a subtype of the type expressed by the right operand.
As a result, it is always a safe operation that can never fail.

### Dynamic cast operator

The dynamic cast operator forces the type of the left operand to be cast as the type expressed by the right operand.
The operation fails with a runtime error if the conversion was actually illegal.
