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
(optionally) compiled to binary code.  The goal of this discussion is
ultimately to choose a semantics for Hylo generics.  We'll present a
few alternatives and would like to discuss the implications of those
choices for the programming model and the language implementation.

We are interested in the insights of experienced generic programmers
regarding the extent to which these choices support the needs of
generic programming and are explained by a workable mental model.

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

When generics are separately type checked, the operations
on generic parameters are restricted to those implied by the bounds on
those parameters.

```hylo
conformance X: Hashable {
  fun hash(into: inout Hasher)
}

f(X())

fun f<T: Equatable>(_ x: T) {
  x.equals(x) // OK
  var h = Hasher()
  x.hash(into: &h) // Error: no 'hash' method on 'x'.
}
```

It doesn't matter that `f` will called on a `Hashable` object; its
bound that `T` is `Equatable` doesn't allow `f` to use a `Hashable`
requirement.  That may seem obvious, but it has important implications
for separate compilation.

Despite the fact that the semantics of generics is thought of as
“static dispatch,” different types will have different implementations
of any trait requirement, and therefore a separately-compiled generic
function must access these implementations via a dynamic mechanism.
(Hylo supports automatic monomorphization as an optimization step that
eliminates the costs of implementation dynamism without changing
semantics, but that can be explicitly disabled for module resilience
purposes and to improve compile times)

Most typically a generic function desugars into a function
that, for each trait bound, additionally accepts a “witness table”
containing implementations of that trait's requirements, so

```hylo
fun f<T: Equatable>(_ x: T) -> Bool {
  return x.equals(x)
}
```

becomes

```hylo
fun f'<T: Equatable>(_ x: T, _ w: EquatableWitness) -> Bool {
  return w.equals(x, x) // dispatch through w
}
```

Other dynamic mechanisms are possible, but witness tables provide the
most predictable performance, and the choice doesn't ultimately affect
the semantic decisions that we are discussing here.  Therefore,
without loss of generality, the rest of this discussion is framed in
terms of witness tables.  The question is, for each call site, how are
witness tables chosen and filled, and how does our choice affect the
ability to provide specialized implementations of requirements?

## Static Invisibility of Concrete Types

In a system that doesn't monomorphize all generics, the compiler never
sees all the concrete types used. For example,

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
occurs only because `f` is called with `Int` (in case you need a
starker example, you can imagine there's a module boundary between `f`
and `g`, so not even whole module analysis can discover `Y<Int>`).

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

For `f1` to use this specialized implementation, it would need to be
passed a specialized witness table for `Y<Int>`'s conformance to
`Equatable`, different from the one for `Y<Bool>` or `Y<String>`. That
in turn would imply that `f`'s separately-compiled implementation
needs to look up the witness table for `Y<T>` dynamically based on
the type of `x`. It can be done, but because the lookup key is
a pair (*trait*, *type*), and both types and traits can be added
arbitrarily, this lookup is much more expensive than looking up a
requirement implementation in a witness table.

## Overlapping Conformances

The above is just one simple example of a cluster of issues that can
arise when conformance declarations overlap.  Two conformance
declarations overlap when, *taken separately*, each one could imply
the conformance of a particular type to a given trait.  These issues
can include ambiguities that are only discovered at runtime:

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
