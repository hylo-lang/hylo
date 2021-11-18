#  Lexicon

### (Storage) Assignment instruction [VIL]
An instruction that assigns a value to a memory location.

### Bound function
A bound function (or a method) is a function that is part of a type, and that is bound to an instance of that type (i.e., the implicit `self` parameter).

A call to a bound function with an *applied type* `T -> U` is actually a call to an unbound function with an *unapplied type* `(Self, T) -> U`, where `Self` is the receiver's type.
In other words, a bound function can be understood as a partial application of an unbound function, binding `self` to an instance.

Note: constructors are not bound functions.
Instead, they are considered being static member functions that accept an implicit `self` parameter.

### Contextual type
A generic type in which all generic type parameters have been substituted by fresh type variables or existential types, depending on the declaration space from which it is been contextualized.

### Fixed type
The expected type of an expression, based on the context in which it appears.
For instance, the fixed type of `9` in `val x: UInt = 9` is `UInt`.
A fixed type is typically the result of an explicit type annotation or of a constraint derived from the use of the expression as an argument.

### Static (member) function
A function that is declared in the context of a type declaration, but that is not bound to instances of that type.
Static functions can access `private` properties and methods.

### (Type) Realization
In the context of type checking, "realization" denotes the creation of the semantic type that corresponds to a declaration, or that is described by a type representation.

### Target
The abstract machine for which Val sources are compiled.
This typically denote either LLVM or VIL's interpreter.

### VIL
Val's Intermediate Language; The IR used by the compiler flow to carry out flow-sensitive analysis, language-specific optimizations, interpretation and code generation.

VIL has multiple stages:
- *Raw VIL* is the output that the VIL emitter produces by walking a typed AST.
- *Checked VIL* is VIL code that has passed typestate anaylsis.
- *Optimized VIL* is VIL code where guaranteed optimizations have been applied.
