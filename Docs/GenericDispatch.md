# Specialization and Dispatching in Generics

Generic Programming in C++, where the discipline was established,
relies in practice on the semantics of the template instantiation
model.  Using overload resolution at instantiation time and class
template specialization, it is natural for a C++ generic programmer to
assume that almost any element of a program can be made available for
customization *post-hoc* based on particular (categories of) type
parameters.

These assumptions cannot hold for Hylo because they conflict with our
commitment that generics can be separately type checked and
(optionally) compiled to binary code with opaque module boundaries.
When we say module boundaries are opaque, we mean that a module's
private implementation details (including the implementations and
layouts of its publicly-exposed generic types and functions) can be
changed without causing recompilation of modules that import it.

## Rationale for separate compilation and type checking of generics

In ordinary programming, the a function's signature tells us which
operations on its arguments the compiler will accept, and once it has
ingested the function's implementation, there can be no further errors
in its body.  But without separate type checking, type errors in a
generic function's body will only be reported when it is *used in a
particular way*. It's very easy to ship an ill-typed generic function
to users, who will be forced to confront type errors in
implementations of which they have no knowledge.  In terms of
programming experience, witing generic code means entering a whole
new—and much more difficult—world.

When generics cannot be separately *compiled*, they become
second-class citizens.  For example, in C++, the API of a
separately-compiled library, or of an application that supports
plugins, is always implemented in terms of concrete types.  Any
generic APIs presented have been constructed on top of these concrete
foundations using a form of manual type erasure, and their
implementations usually cannot be changed without breaking clients.

In general, separate compilation limits the compiler's ability to make
optimizations, because it cannot see across the separate compilation
boundary.  Maintaining performance depends on doing enough computation
*within* a compilation boundary that the cost paid to cross the
boundary becomes insignificant.  Unfortunately, that's not always
possible with generic code: because a heavily-used concept requirement
(whose implementation is often as lightweight as an integer increment)
may lie across such a boundary from the algorithm that calls it.
Whether to pay these costs in exchange for the benefits of separate
compilation—where it is useful—is a choice for programmers to make,
not language designers.

## Basics of the Hylo Design for Generics

The basic model for Hylo generics is very much like that of Swift and
Rust:

- A concept applies to a single type, not some tuple of types.
- The concept's requirements are specified in `trait`s.
  - Method requirements are specified in a `trait` by ordinary
    signatures that must be matched exactly by implementations in a
    `conformance`.
  - Associated type requirements can be specified, optionally with
    conformance and type equality bounds.
- Trait requirements can have default implementations.
- Traits can refine other traits
- Generic type parameters can have conformance and type equality bounds.
- All conformances of types to traits are explicitly specified
- APIs available on generic parameters are restricted to those given
  by their bounds in that context.

(Details, capabilities, and syntax sugars irrelevant to this
discussion are omitted).

Examples:

```hylo
trait Equatable {
  fun equals(_: Self) -> Bool // method requirement
}

trait Hashable: Equatable { // trait refinement
  fun hash(into: inout Hasher)
}

trait Iter {
  type Element // associated type requirement
  fun next() inout -> Optional<Element>
}

trait Sequence {
  type Iterator: Iter // associated type conformance bound

  fun iterator() -> Iter

  fun equals<Other: Sequence>(_: Other) -> Bool
    where Self.Element == Other.Element // type equality bound

  // requirement with default implementation
  fun nth(_ n_: Int) -> Optional<Iter.Element> {
    var i = iterator()
    for n in 0..<n_ { _ = i.next() }
    return i.next()
  }
}

type X { var a: Int }
conformance X: Equatable {
  fun equals(_ other: X)-> Bool { return self.a == other.a } // requirement implementation
}

type Y<T> {
  var a: T
}

conformance Y: Equatable where T: Equatable { // conditional (bounded) conformance
  fun equals(_ other: Y)-> Bool {
    return self.a.equals(other.a) // use of trait requirement available via to bound
  }
}
```

## Separate Compilation and Dispatch

When generics are separately type checked, the operations on generic
parameters are restricted to those implied by the bounds on those
parameters.

```hylo
conformance X: Hashable {
  public fun hash(into: inout Hasher)
}

fun f<T: Equatable>(_ x: T) {
  x.equals(x) // OK
  var h = Hasher()
  x.hash(into: &h) // Error: no 'hash' method on 'x'.
}

f(X())
```

It doesn't matter that `f` will called on a `Hashable` object; its
bound that `T` is `Equatable` doesn't allow `f` to use a `Hashable`
requirement.  That may seem obvious, but separate *type checking* has
important implications for modular *separate compilation*, because the
implementation of a generic function can only depend on its declared
requirements and the specific types being passed to it.

### Compilation Model

Despite the fact that the *semantics* of generics is thought of as
“static dispatch,” different types will have different implementations
of any trait requirement, and therefore a separately-compiled generic
function must access these implementations via a dynamic *mechanism*.
(Hylo supports an optimization step that eliminates the costs of
implementation dynamism without changing semantics, but that step can
be explicitly disabled to improve compile times and to allow for
binary-compatible evolution of library implementations)

Most typically a generic function desugars into a function
that, for each trait bound, additionally accepts a “witness table”
containing implementations of that trait's requirements, so

```hylo
fun f<T: Equatable>(_ x: T) -> Bool {
  return x.equals(x)
}
```

desugars to

```hylo
fun f<T: Equatable>(_ x: T, _ w: EquatableWitness) -> Bool {
  return w.equals(x, x) // dispatch through w
}
```

Other mechanisms are possible, but witness tables provide  predictable
performance, and the choice doesn't ultimately affect the semantic
decisions that we are discussing here.  Therefore, without loss of
generality, the rest of this discussion is framed in terms of witness
tables.

### Static Invisibility of Concrete Types

The process by which languages like C++ and Rust take generic
(type-polymorphic) components and produce concrete implementations for
the specific types passed is called *monomorphization*.  It involves
following concrete types passed into a tree of generic component uses
until they bottom out in concrete code again at the leaves.  Rust
generics  are separately type checked, so that process happens before
monomorphization, while in C++, it happens afterwards.

When generics are separately *compiled*, by contrast, the implementation
details of a generic component are unavailable to the compiler at its
use-sites.  Only the declaration of that component is available, so
the compiler cannot follow a type through the tree its leaves;
although some monomorphization may be possible, that process must stop
at a separate compilation boundary.

In a system that doesn't monomorphize all generics, the compiler never
sees all the concrete types used. For example, here

```hylo
fun f1<U: Equatable>(_ w: U) -> Bool {
  return w.equals(w)
}

fun f<T>(_ x: T) -> Bool {
  return f1( Y<T>(x) )
}

fun g() {
  let a = Y<Bool>(false)
  print(a.equals(a))
  print(f(3))
}
```

The compiler encounters `Y<Bool>` directly, but not `Y<Int>`, which
occurs only because `f` is called with `Int`. In case you need a
starker example, you can imagine there's a module boundary between `f`
and `g`, which are built separately, so not even whole module analysis
can discover `Y<Int>`.

This invisibility interacts deeply with requirement
specialization. Imagine that we are able to supply a
specialized implementation of `equals` just for `Y<Int>`:

```hylo
conformance Y<Int>: Equatable {
  fun equals(_ other: Self) -> Bool {
    return true // All Y<Int>s are equal
  }
}
```

For `f1` to use this specialized implementation, there are two possibilities:

1. `f1` would need to be passed a specialized witness table for
   `Y<Int>: Equatable` that is different from the one for `Y<Bool>` or
   `Y<String>`. That, in turn, would imply that `f`'s
   separately-compiled implementation needs to look up the witness
   table for `Y<T>: Equatable` dynamically based on the type of `x`.

2. The `equals` entry in the generic `Y<T>: Equatable` witness
   table—the one that applies for all `T`s—would need to perform some
   kind of dynamic dispatch based on the type of `x`.

## Overlapping Conformances

The above is just one simple example of a cluster of issues that can
arise when conformance declarations overlap.  Two conformance
declarations overlap when, *taken separately*, each one could imply
the conformance of a particular type to a given trait. Because of
visibility boundaries,
%%
The hardest
problems to deal with are ambiguities that are only discovered at
runtime:

```hylo
trait P {}
trait Q {}
trait R {
  fun f()
}
conformance Y: R where T: P { fun f() { print "A"} }
conformance Y: R where T: Q { fun f() { print "B"} }

fun callf1<T: R>(_ c: T) { c.f() }
fun callf<U>(_ d: U) {
  callf1(Y<U>(d)) // conformance of Y<U> to R is ambiguous when U == X
}

conformance X: P {}
conformance X: Q {}

callf(X())
```

We could resolve this ambiguity by arbitrarily (deterministically)
picking one conformance or another, but even then, the cost of
maintaining data structures for the kind of dynamism discussed here

.
Unfortunately, in the general case, the information that would allow
this kind of dynamism, and the need for it,  may lie beyond an opaque
module boundary.  Ultimately building the


--------------------------

## Possible Solutions

### Ban Overlapping Conformances

Banning overlapping conformances means

## Generic Type Specialization

In C++ one expects to be able to completely redefine a generic type
based on its type parameters.  A specialization might have nothing in
common with its unspecialized definition (which might not even
exist!).  That capability is clearly incompatible with separate
typechecking.

We can, however, consider allowing the conformance of a (generic) type
to a given trait to be specialized:

```
conformance Y: Trait1 {
  fun requirement1() { ... }
}

conformance Y: Trait1 where T: Trait2 {
  fun requirement1() { ... } // specialized implementation of requirement1
}
```

A number of problems can arise from this sort of specialization.

### Associated Type Requirements

If this kind of specialization can change how associated type requirements are satisfied,

a system to distinguish the same spelling of a generic type in
two different contexts, allowing associated types of type parameters
to be stored conflicts with soundness

## Post-hoc Algorithm Specialization

So far in the examples we've seen, all customization points have been
specified by trait requirements.  That means, after a trait has been
established, an algorithm written in terms of that trait is *not* a
customization point.
