# Transformation declarations

This document is inspired by a Swift proposal on [in-place operations](https://github.com/apple/swift/blob/main/docs/proposals/Inplace.rst).
Its goal is to provide syntactic support to harmonize the handling of transformative operations.

## Background

There exist two patterns to express transformative operations in Val.
The first and perhaps most intuitive is to mutate a value.

```val
var a = T()
transform_1(&a)
```

A second pattern takes inspiration from pure functional programming and expresses transformation as the application of a function producing the transformed value.

```val
let a = T()
let b = transform_2(a)
```

The latter pattern might cost a copy but comes with two advantages.
First, it is applicable even if `a` is immutable, as in the example.
Second, it supports non-homomorphic operations, whereas mutation is type-preserving.

One may wonder: what is the passing policy of `transform_2`'s first parameter?

If `a` is passed by value, then `transform_2` must copy some of its contents to initialize `b`'s value.
If the call is `a`'s last use, though, then such copies are unnecessary, because relevant parts of `a` could have been reappropriated by `b`.
A better strategy would have been to consume `a`.

If `a` is consumed but the call is *not* `a`'s last use, an explicit copy is required.
Such a copy is inefficient, though, because the transformation might not require any parts of `a`.
A better strategy would have been to pass `a` by value.

There is a last use case of interest for homomorphisms.

```val
var a = T()
a = transform_3(a)
```

Here, passing `a` by value results in an unnecessary temporary copy before the reassignment.
If it is consumed, then `transform_3` is equivalent to a function mutating its argument in-place, just like `transform_1`.

These observations suggest that the same transformation might be best expressed in three different ways, depending on the context in which it is applied.

- If `a` is mutable and the transformation is homomorphic, then we should apply a function `(inout T) -> Unit`.
- If `a` is not mutated and is used after the transformation, then we should apply a function `T -> U`
- If `a` is not used after the transformation, then we should apply a function `(sink T) -> U`

## Problem

We identify two problems from the above observations.

### Choosing the right variant

Assuming all three kinds of transformations are defined, choosing the correct variant depending on the situation requires a deep understanding of Val's semantics.
Further, unless a compiler can figure out the relationship between variants, it is likely to produce sub-optimal suggestions to fix ill-typed programs.

```val
fun foo_1(_ x: T) -> U { ... }
fun foo_2(_ x: sink T) -> U { ... }

let a = T()
let b = foo_2(a) // error: 'a' is used after being consumed, consider copying it.
print(a)
print(b)
```

In this example, the compiler detects a use of `a` after it has been moved and suggests an explicit copy of `a`.
This suggestion is ill-advised because the correct fix would have been to call `foo_1` rather than `foo_2`.

### Defining APIs

What about the name of the variants?

One might be tempted to rely on a naming convention.
In Swift, for instance, the mutating variant of the sorting operation on collections is named `sort` while the one creating a new independent value is called `sorted` (Swift does not have a `sink` parameter policy).

The problem with that approach is to determine a uniform naming convention capable of expressing all kinds of transformations concisely and elegantly.
Further, naming conventions do not formalize the relationship between related methods that a compiler may exploit for synthesizing common patterns or automating substitutions.

## Proposed solution

We propose to address both issues by defining a formal naming convention to identify related transformations, along with syntactic support at the call site to clarify the intent of a specific call.

### Formalize the relationship

All variants of the same transformation should be expressed with a single syntactic construct that formalizes their relationship.
We call such a construct a *transformation declaration*.

```val
fun sort {
  (_ x: T) -> T { ... }
  (_ x: sink T) -> T { ... }
  (_ x: inout T) { ... }
}
```

Here, `sort` has a definition for each variant where:
- `T -> U` is called the non-mutating variant;
- `(sink T) -> U` is called the consuming variant; and
- `(inout T) -> Unit` is called the mutating variant.

Each variant has the same name and the compiler is responsible for selecting the appropriate one depending on the situation.
It can easily distinguish between a mutating and a non-mutating variant because `&` is required to prefix `inout` arguments.

```val
var a = T()
sort(&a) // calls '(inout T) -> Unit'
```

The compiler can also identify the last use of a particular argument and therefore select `(sink T) -> U` over `T -> U` whenever appropriate.

```val
var a = T()
let b = sort(a) // calls 'T -> T'
let c = sort(a) // calls '(sink T) -> T'
```

Methods pose another problem because the language does not have syntax to differentiate calls to non-mutating methods from calls to mutating ones.
We propose to solve with a new operator `.=`.

```val
var a = T()
let b = a.sort() // calls `() -> T`
a.=sort()        // calls `mutating () -> T`
```

If a transformation declaration does not feature all three variants, the compiler falls back on the most appropriate choice.
A program is ill-typed if the transformation declaration does define a non-consuming variant, yet it is called before the last use of its argument.
In that case, the compiler identifies a standard use-after-consumption error.

```val
fun reverse {
  (_ x: sink T) -> T { ... }
  (_ x: inout T) { ... }
}

var a = T()
let b = reverse(a) // error: 'a' is used after being consumed, consider copying it.
let c = reverse(a)
```

For the sake of consistency, we impose the following rules on transformation declarations.
1. It cannot contain more than three definitions.
2. All definitions must have a distinct type.

The notional type of the transformation depends on the type of its variants.
- A non-consuming variant `T -> U` implies the transformation has type `T -> U`.
- A consuming variant `(sink T) -> U` implies the transformation has type `T -> U`.
- A mutating variant `(inout T) -> U` implies the transformation has type `T -> (T, U)` if `U` is not `Unit`. Otherwise, it has type `T -> Unit`.

We require that the type of each variant be consistent with the type of the containing transformation.
For instance, the following transformation declaration is ill-typed.

```val
fun invalid_transform {
  (_ x: T) -> U { ... }
  (_ x: sink T) -> X { ... } // error: inconsistent variant type: transformation has type 'T -> U'.
}
```

### Synthesizing the mutating variant

Given a non-consuming variant `foo` of type `T -> (T, U)`, there is a corresponding mutating variant `(inout T) -> U`.

```val
(_ x: inout T) -> U {
  var y: U
  (x, y) = foo(x) // calls the user-defined variant
  return y
}
```

Given a non-consuming variant `foo` of type `T -> T`, there is a corresponding mutating variant `(inout T) -> Unit`.

```val
(_ x: inout T) {
  x = foo(x) // calls the user-defined variant
}
```

Given a consuming variant `foo` of type `(sink T) -> (T, U)`, there is a corresponding mutating variant `(inout T) -> U`.

```val
(_ x: inout T) {
  var y: U
  (x, y) = foo(x) // calls the user-defined variant
  return y
}
```

Given a consuming variant `foo` of type `(sink T) -> T`, there is a corresponding mutating variant `(inout T) -> Unit`.

```val
(_ x: inout T) {
  x = foo(x) // calls the user-defined variant
}
```

If both a non-consuming and a consuming variant are defined, the mutating variant is synthesized with the latter.

### Handling operators

The proposed formal naming convention does not generalize to operators, because mutating operators typically have a different name than their non-mutating counterparts.
For instance, `+=` is the mutating operator of `+`.

We propose to teach the compiler about the relationship between operators to tackle this issue.
The rest of this proposal should apply equally to operator methods with one exception: operators are not accessible via the `.` accessor and should not be accessible via `.=` either.

### Relationship with overloading

To prevent ambiguities during type inference, we propose to forbid a function `T -> U` to overload a transformation of the same name.

```val
 fun sort {
   (_ x: sink T) -> T
 }
 fun sort(_ x: sink T) -> U // error: cannot overload 'sort' with '(sink T) -> U'
                            // because there exists a transformation 'T -> T'
 ```

## Open questions

### Transformations with arguments

In the current form, this proposal does not address how the compiler should behave when the transformation has more than a single argument.
Particular cases of interest include transformation may consume additional arguments.

```val
fun filter {
  (_ x: T, where predicate: T -> Bool) -> T { ... }
  (_ x: sink T, where predicate: T -> Bool) -> T { ... }
  (_ x: inout T, where predicate: T -> Bool) { ... }
}

let a = T()
let p: T -> Bool = fun (_ t) { true }
let b = filter(a, where: p)
```

Here, the call to `filter` is `a` and `p`'s last use.
So, it would make sense to define a specialized variant that consumes both arguments.

Intuitively, the relationship between `(sink T, P) -> T` and `(inout T, sink P) -> Unit` seems primordial than between `(sink T, P) -> T` and `(inout T, P) -> T`.
Further, if the second argument should be part of the transformation, then one may just assume the transformation is of type `(T, U) -> V` and the mechanisms laid out in this proposal apply.
From these observations, one option to deal with additional arguments might be to just overload the transformation and let the compiler select the appropriate candidate depending on the context of the call.
