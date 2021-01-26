#  Built-ins

This document describes the current vision for the implementation of built-in types.

## Overview

Most programming languages provide a set of built-in types defined directly within their compilers, to serve as building blocks to define operations and more complex data structures.
This set typically consists of primitive data types, such as Booleans value, numbers, characters and pointers, but may also include more sophisticaed types, such as tuples and first-class functions.

Built-in types are sometimes considered being part of a language's *standard library*.
The latter is a collection of common data types, such as arrays and hash tables, which is distributed with the language and and its compiler.
The standard library can and often is written in the language itself, providing a seamless interface with the user code that consumes it.
But this approach begs two question:
1. How built-in types are represented within the compiler?
2. How do built-in types interact with the standard library and, eventually, user code?

Val's implementation aims to implement as much as possible of the library in Val directly.
The goal is to keep the size of what must be "hard-coded" into the compiler to a strict minimum.
For instance, this means that the `Val::Int` type should be a regular Val type, declared in the standard library, and that the compiler should use as little magic as possible to map it onto a built-in concept of integer values.

## The `Builtin` module

The standard library relies on a built-in module (named `Builtin`), which defines a set of types and functions.
This module and its contents are generated directly within the compiler, as part of an AST context.
Built-in types are treated differently than other nominal types.
They are pure value objects, which do not contain any constructor, methods or destructors.
They can only be created out of literal expressions, or result from a built-in function call (e.g., `Builtin::i64_add(_:_:)`).

The correspondance between a built-in type (e.g., `Builtin::i64`) and an actual type (e.g., `Val::Int`) is achieved by leveraging Val's view system.
Literal expressions are given the type `Builtin::***Literal`, where the three stars stand for either `Int` or `Float`.
They do not convey the semantics of the expressed value's type, such as its signedness or bit width.
This is determined by context. 
Literal expressions are assignable to any compatible built-in value type.
For example, an expression of type `Builtin::IntLiteral` is assignable to a variable of type `Builtin::i64`.
This defines the interpretation of the literal (e.g., that it is a 64-bit signed integer).
Furhtermore, literal expressions are also *convertible* to any type that conforms to the view `ExpressibleBy***Literal`.
The latter requires conforming types to implement an initializer that accepts a built-in literal value.
Hence, the most simple implementation of `Val::Int` can be written as follows:

```val
type Int: ExpressibleByIntLiteral {

  new (literal value: Builtin::IntLiteral) {
    self.value = value
  }

  var value: Builtin::i64

}
```

This is a product type with a single property `value` of type `Builtin::i64`.
The type's constructor satisfies the only requirement for its conformanc to `ExpressibleByIntLiteral`, by simply assigning the value of a literal expression to the property `value`. 

