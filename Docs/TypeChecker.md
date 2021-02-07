# Type Checker

This document gives a brief overview of Val's static type checker.
The main entry point for is the `TypeChecker` class, defined in `Sources/Sema/TypeChecker.swift`.

## Type Checking and Type Inference

__Type checking__ is the process that consists of verifying that a given program follows the rules of the language's type system.
Val is statically typed, meaning that type checking is performed during compilation (in contrast to dynamically typed languages, that perform type checking during the program's execution).
Static type checking requires the compiler to know the type of each expression in the program.
This can be provided in the form of explicit type annotations, or *inferred* from the program itself.
The latter approach is commonly called __type inference__.

Although some languages are able to infer all type annotations (e.g., OCaml), Val chooses not to and requires explicit type annotations on most declarations.
The benefit of this approach is twofold.
First, it upholds local reasoning, whereas "full" type inference is much more global and thus more difficult to predict.
As a result, type errors are notoriously difficult to understand.
Second, it supports modularity.
Declarations can be considered as relatively independent constructs, that can be parsed, type-checked or even compiled individually.

Nonetheless, Val uses type inference to elide type annotations wherever these are (relatively) obvious and would only pollute the source text, as well as to disambiguate the use of overloaded symbols.
For instance, consider the following program:

```val
fun inc(_ i: Int) -> Int { ret i + 1 }
fun inc(_ i: Float) -> Float { ret i + 1.0 }
val x = inc(41)
```

In the above example, type annotations serve to specify the type of both functions explicitly.
From these annotations, the compiler is able to determine that the first function is typed as `Int -> Int`, whereas the second is typed as `Float -> Float`.
Type inference is used at the third line, both to infer the type of the value `x`, but also to determine which version of the function `inc` the user

## Implementation Overview

Conceptually, type checking is a composition of multiple phases:
- **Extension binding**
  Binds extensions to the declaration they extend.
- **Conformance enumeration**
  Initializes the view conformance set of all nominal types. This includes listing inherited and synthetized conformances.
- **Existential realization**
  Realizes existential types from generic type signatures.
- **Name resolution**
  Resolves type and variable identifiers to their declaration.
- **Semantic type checking**
  Checks that a particular declaration satisfies Val's type system.

Note that the compiler does not perform these phases completely sequentially, not only because of performance concerns, but also because some operations may require results from "later" phases.
Consider the following example, which is perfectly legal in Val:

```val
type A {
  type B {
  }
  val c: B::C
}

extn A::B {
  type C {}
}
```

Binding the extension requires a qualified name lookup within the declaration space defined the type `A`.
However, notice that building `A`'s member lookup table involves resolving `B::C` (the signature of the property `c`), which happens to be defined within the extension.

To tackle these kinds of issues, the type checker operates as lazily as possible, so that not all of the AST need to be brought up to a particular phase at the same time.
This process is "declaration-driven"; it starts at a declaration node (e.g., a `Module`) and visits all nested declarations recursively.
Dependencies are not fully type checked.
Instead, the type checker aims to move them at the minimal "phase" that satisfies the requirements of the construction it is checking.
For instance, referring to a method in another type does not triggers said type to be fully type checked; it is sufficient to build its member lookup table and realize the signature of the method.

In the above example, the signature `B::C` of the property `c` will be assigned a temporary "unresolved" type and will not be processed until the checker encounters an expression that refers to it, or the type `A` is scheduled to be fully checked.
This means that extension binding can be delayed until the type `A::B` is involved in some other type checking operation.

## Constraint generation

This step consists of walking the AST to generate type constraints from expressions.

### Literals

Literal expressions are given a type variable which the solver will try to substitute for a type that conforms to `ExpressibleByBuiltin*Literal` (defined in the standard library).
During constraint solving, the solver uses a default conforming type for each literal as an escape hatch if surrounding context doesn't provide enough information to infer the conforming type.

## Constraint solving

This step consists of solving the generated equations, through type inference.

## Solution application

This pass consists of applying the solution computed by the previous pass to the AST.
