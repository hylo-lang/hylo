# Assignment methods

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

The latter pattern costs a new assignment but comes with two advantages.
First, it is applicable even if `a` is immutable, as in the example.
Second, it supports non-homomorphic operations, whereas mutation is type-preserving.

One may wonder: what is the passing policy of `transform_2`'s first parameter?

If `a` is passed by value, then `transform_2` must copy some of its contents to initialize `b`'s value.
If the call is `a`'s last use, though, then such copies are unnecessary, because relevant parts of `a` could have been reappropriated by `b`.
A better strategy would have been to consume `a`.

If `a` is consumed but the call is *not* `a`'s last use, an explicit copy is required.
Such a copy is inefficient, though, because the transformation might not require *all* parts of `a`.
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
- If `a` is immutable and used after the transformation, then we should apply a function `T -> U`
- If `a`is not used after the transformation, then we should apply a function `(sink T) -> U`

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

### One name

All variants of the same transformation should be expressed with one single name and rely on overloading resolution to select the appropriate candidate.
That approach can easily distinguish between a mutating and a non-mutating variant because `&` is required to prefix `inout` arguments.

```val
let b = sort(a) // non-mutating
sort(&a)        // mutating
```

The compiler is currently unable to resolve overloading between `T -> U` and `(sink T) -> U` since the language does not have syntax to denote consumption at the call site.
Fortunately, it can identify the last use of a particular argument and therefore select `(sink T) -> U` over its counterpart whenever appropriate.

To preserve an intuitive type inference, though, we propose to forbid a method `T -> U` to be overloaded with a method `(sink T) -> V`.

```val
fun sort(_ x: T) -> U
fun sort(_ x: sink T) -> V // error: cannot overload 'sort' with '(sink T) -> V'
                           // because there exists a candidate 'T -> U'
```

Methods pose another problem because the language does not have syntax to differentiate calls to non-mutating methods from calls to mutating ones.
We propose to solve with a new dedicated accessor `.=`.

```val
var a = T()
let b = a.sort() // non-mutating
a.=sort()        // mutating
```

### Synthesizing variants

Given one specific transformation variant, the compiler might be able to synthesize default implementation for the other variants.

While we describe the implementation of synthesized functions in Val, note a compiler may generate VIL directly.
Hence, synthesized implementations do not need to adhere to the above-mentioned strategy to select overload candidates.

#### Synthesizing the consuming variant

Given a user-defined function `foo` of type `T -> U`, there is a corresponding function `(sink T) -> U`.

```val
fun foo(_ x: sink T) -> U {
  return foo(x) // calls the user-defined function
}
```

Given a user-defined function `foo` of type `(inout T) -> U`, there is a corresponding function `(sink T) -> U`.

```val
fun foo(_ x: sink T) -> U {
  var y = x
  foo(&y) // calls the user-defined function
  return y
}
```

#### Synthesizing the non-mutating, non-consuming variant

Given a user-defined function `foo` of type `(sink T) -> U` where `T` is `Copyable`, there is a corresponding function `T -> U`.

```val
fun foo(_ x: T) -> U {
  let y = x.copy()
  return foo(y) // calls the user-defined function
}
```

Given a user-defined function `foo` of type `(inout T) -> U` where `T` is `Copyable`, there is a corresponding function `T -> U`.

```val
fun foo(_ x: T) -> U {
  var y = x.copy()
  foo(&y) // calls the user-defined function
  return y
}
```

#### Synthesizing the mutating variant

Given a user-defined function `foo` of type `T -> T`, there is a corresponding function `(inout T) -> Unit`.

```val
fun foo(_ x: inout T) {
  x = foo(x) // calls the user-defined function
}
```

Given a user-defined function `foo` of type `(sink T) -> T`, there is a corresponding function `(inout T) -> U`.

```val
fun foo(_ x: inout T) {
  x = foo(x) // calls the user-defined function
}
```

### Handling operators

The proposed formal naming convention does not generalize to operators, because mutating operators typically have a different name than their non-mutating counterparts.
For instance, `+=` is the mutating operator of `+`.

We propose to teach the compiler about the relationship between operators to tackle this issue.
The rest of this proposal should apply equally to operator methods with one exception: operators are not accessible via the `.` accessor and should not be accessible via `.=` either.
