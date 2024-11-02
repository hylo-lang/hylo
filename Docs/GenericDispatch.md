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
private implementation details—including the layouts of its public
types and implementations of its public functions, generic or not—can
be changed without recompiling its clients.  (Hylo's implementation
techniques for efficient binary-compatible type evolution are largely
taken from Swift.)

## Rationale for separate compilation and type checking of generics

In ordinary programming, a function's signature tells us which
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

Hylo supports an optimization step that eliminates the costs of
separate compilation without changing semantics, but that optimization
can be selectively disabled:

## Basics of the Hylo Design for Generics

The basic model for Hylo generics is very much like that of Swift and
Rust:

- globally, to improve compile times; typically for a debug build.
- at select module boundaries, to allow binary-compatible library
  evolution.

- A concept applies to a single type, not some tuple of types.
- The concept's requirements are specified in `trait`s.
  - Method requirements are specified in a `trait` by ordinary
    signatures that must be matched by implementations in a
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
    where Self.Element == Other.Element, // type equality bound
          Self.Element: Equatable

  // requirement with default implementation
  fun nth(_ n_: Int) -> Optional<Iter.Element> {
    var i = iterator()
    for n in 0..<n_ { _ = i.next() }
    return &i.next()
  }
}

type X { var a: Int }
conformance X: Equatable {
  // requirement implementation
  fun equals(_ other: X)-> Bool { return self.a == other.a }
}

type Y<T> {
  var a: T
}

conformance<T> Y<T>: Equatable where T: Equatable { // conditional (bounded) conformance
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

A separately-compiled generic function has a single implementation in
object code that can be used regardless of the function's generic
parameters.  Therefore, despite the fact that the *semantics* of
generics is thought of as “static dispatch,” the compiled code must
access the implementations of trait requirements, which depend on
generic parameters, via a dynamic *mechanism*.

In most languages supporting separate compilation, a generic function
desugars into a function that, for each trait bound, additionally
accepts a “witness table” containing implementations of that trait's
requirements, so

```hylo
fun f<T: Equatable>(_ x: T) -> Bool {
  return x.equals(x)
}
```

desugars to

```hylo
fun f<T>(_ x: T, _ w: EquatableWitness) -> Bool {
  return w.equals(x, x) // dispatch through w
}
```

Every conformance declaration desugars into the declaration of a
constant witness table, so

```hylo
conformance X: Equatable {
  fun equals(_ y: Self) -> Bool { return self.a == y.a }
}
```

becomes:

```hylo
let _x_equatable_witness = EquatableWitness(
  fun (_ y: Self) -> Bool { return self.a == y.a }
)
```

You can think of the witness table as the reification of the
conformance.

Other mechanisms are possible, but witness tables provide  predictable
performance, and the choice of mechanism doesn't ultimately affect the
semantic decisions that we are discussing here.  Therefore, without
loss of generality, the rest of this discussion is framed in terms of
witness tables.

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
the compiler cannot follow a type through the tree to its leaves;
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

For `f1` to use this specialized implementation, `f` would need to
pass it a specialized witness table for `Y<Int>: Equatable`, different
from the more general one used for `Y<Bool>` or `Y<String>`.
Therefore, the implementation of `f` would have to *dynamically* look
up the `Equatable` witness table for `Y<T>` based on what `T` turns
out to be.

## Overlapping Conformances

The above is one simple example of overlapping conformances.  In
general, two conformance declarations overlap when, *taken
separately*, each one could imply the conformance of a particular type
to a given trait. In this case, `Y<T>: Equatable` (for all `T`)
overlaps with the more-specific conformance `Y<Int>: Equatable`,
because when `T` is `Int`, either one, taken alone could apply.

### Runtime Costs

Ideally, the language would always select the most specific
conformance applicable for any given type, but as we saw above, that
implies additional dynamism.  In general, the algorithm implementing
that selection is approximately as expensive as overload resolution, a
cost we do not want to pay at runtime.  That cost can be mitigated by
adding a global cache, which would require dynamic allocation and
synchronization for thread safety.

### Ambiguities

In the worst case, the algorithm would need to resolve an
ambiguity between equally-specific conformances that can only be
discovered at runtime.  Here's an example:

```hylo
// Int conforms to both P and Q
trait P {}
trait Q {}
conformance Int: P {}
conformance Int: Q {}

type X<T> {}
trait R { static fun id() -> String }

// X conforms to R differently depending on T's conformance to P or Q.
conformance<T> X<T>: R where T: P { static fun id() { return "P" } }
conformance<T> X<T>: R where T: Q { static fun id() { return "Q" } }

// Prints `id´ from R conformance
fun print_id0<SomeR: R>() { print(SomeR.id()) }

// Looks up conformance `X<U>: R`
fun print_id<U>() {
  print_id0<X<U>>()  // ambiguous lookup when `U` is `Int`
}

print_id<Int>()
```

We *could* resolve this ambiguity one of two ways:
- **Signal an error dynamically.**  This answer undermines the value of
  separate type-checking.  In some sense it's worse than C++—which
  produces a usually-inscrutable compiler error in cases like
  these—because the error is “shifted right” to runtime.
- **Arbitrarily pick one** conformance or another at runtime. Making
  the choice deterministic is difficult and the unpredictable behavior
  is hard to justify.

### Soundness Problem #1: Layout Conflicts

The next problem with overlapping conformances is that it's possible
to define a generic type whose layout depends on the way a generic
parameter conforms to some trait.  For example,

```hylo
type X<T: P> {
  var stored: T.Q
  fun f(x: T) { ... }
}
```

If a given `T` can conform to P in multiple ways, `T.Q` could be
different in each conformance. In the example below, what is the size
of the result of `f<Int>()`?  It depends which of the two conformances
`Int: P` is in effect.

```hylo
// module A
conformance Int: P { type Q = String }
// module B
conformance Int: P { type Q = Bool }
// module C
fun f<T: P>() -> X<T> { return X<T>() }
```

### Soundness Problem #2: Invariant Conflicts

Imagine a type `SortedArray<T: Comparable>` that maintains the
invariant that its elements of type `T` are stored in increasing
order.  It's easy to see how different `Comparable` conformances,
which supply an ordering for `T`, could lead to a `SortedArray<T>`
with an ordering that breaks the invariants of the same type in a
different context.  This example is important because unlike type
layout information, whose conflicts could in theory be detected,
information about invariants is completely beyond the reach of the
compiler.

## What if we just Ban Overlapping Conformances?

Rather than paying the performance and predictability costs of always
choosing the best-matching overlapping conformance, we *could* ban
them. Rust does that by:
- Disallowing specialized conformances.
- An “orphan rule” requiring `T: P` to be defined in either the module
  of `T` or that of `P`, and making it an error to import a module if
  the imported and importing modules both declare `T: P`.

### No Generalized Post-Hoc Conformance

The “orphan rule” prevents generalized post-hoc conformances: if the
standard library provides `Int` and `Monoid`, but no conformance `Int:
Monoid`, the programmer can't declare it themselves.  Also, no matter
where they are defined, the two standard conformances ((operation:
`+`, identity: `0`) and (operation: `*`, identity: `1`)) cannot
coexist.

### No Specialization

Those limitations can be waved away as insignificant in practice, but
the inability to express specialization is a more serious problem for
generic programming. For any trait requirement, one could declare a
default implementation and optionally another for any given generic or
non-generic type. More specific implementations that depend on further
trait or generic parameter constraints would be disallowed:

```hylo
trait P {
  type Q
  // Requirement with empty default implementation
  fun f() {}
}
type X<T> { }

// X conforms to P for all Ts.
conformance<T> X<T>: P {
  type Q = T
  // Specific implementation for X<T> for any T
  fun f() { print("hi") }
}

// X<T> conforms to P differently when T is Equatable
conformance<T> X<T>: P where T: Equatable {  // Disallowed
  fun f() { print("bye") }
}

// Specialized implementation of f for P when P.Q is Equatable
specialization P where Q: Equatable {        // Disallowed
  fun f() { print("why?") }
}
```

Notably, the first disallowed example is almost implied by the syntax
of something we want to support: the post-hoc definition of a
specializable algorithm given a set of trait requirements. Below,
`some_algorithm` has a default implementation that depends only on the
requirements of `P`, but has a specialized implementation when the
conforming type also conforms to `Equatable`.

```hylo
trait SomeAlgorithm {
  fun some_algorithm()
}
conformance<T> T: SomeAlgorithm where T: P {
  fun some_algorithm() { ... } // default implementation
}
specialization<T: P & Equatable> T: SomeAlgorithm {
  fun some_algorithm() { ... } // specialized implementation
}
```

### No Local Conformances

The most significant effect of an overlapping conformance ban is that
two modules each defining a local conformance `X: P` can't
coexist. The “orphan rule” described
[above](#no-generalized-post-hoc-conformance) is probably the most
effective way to prevent that from happening.  Any other approach we
know of allows situations where an update to module X can't be used
because of a conformance conflict with some unrelated module.  These
conformances might not even be visible in the module's public API, but
they would have to appear in diagnostics, which is never a good
look. Avoiding diagnostics that point to implementation details
is a primary motivator for separate type-checking of generics, after all.

The ability to write a conformance at *function scope* using local
values in its implementation, along with [implicit function
parameters](https://docs.scala-lang.org/tour/implicit-parameters.html)—a
feature Hylo takes from Scala—would solve a convenience problem
created by strict mutable value semantics: sometimes operations on a
part of a structure require access to the whole structure.  In a
reference-semantic languages, one might simply store a reference to
the whole inside the part, but in a simple language with no
first-class references, one must explicitly pass the whole structure
around and access it with cumbersome repetitive syntax.  Examples
require too much background to explain here, but suffice it to say
that locally declared conformances will tend to overlap, and we think
this use case is important.

## What Hylo Does Instead: Static Choice

The remaining alternative, generally, is to give up the idea that a
generic function will always dispatch to the most specific conformance
that can match the concrete type to which a function's type parameter
is bound.

The particular way Hylo chooses witness tables is called “static
choice.”  The rule is simple: when a new witness table is required,
the conformance used is the most-specific one *statically visible* at
the point of use, and if there is no unique most-specific conformance,
it is an ambiguity error.

The rest of this document discusses the implications of that simple rule.

Note the use of the term *new* above. If one constrained generic
function calls another, passing along its generic parameter, no new
witness table is needed; the one for that parameter is passed along:

```hylo
fun is_really_equal<T: Equatable>(_ x: T, _ y: T) -> Bool {
  return x == y
}

fun is_equal<T: Equatable>(_ x: T, _ y: T) -> Bool {
  // implicit witness parameter for `T: Equatable` passed along
  return is_really_equal(x, y)
}

let yes = is_equal(1, 1) // New witness table for `Int: Equatable` needed.
```

### How the Ambiguity Problem Is Solved

Static choice makes statically-detectable ambiguities into
compile-time errors, and because it wilfully ignores conformances that
can only be seen dynamically and chooses the best statically-visible
one, there's nothing to detect dynamically.  Of course at this point
it remains to be seen how well this behavior—which doesn't match the
one we earlier called “ideal”—supports generic programming.

### Solving Soundness Problems

Both soundness problems we've discussed boil down to one issue: the
meaning of a generic type changes based on how the conformance
constraints on its parameters are satisfied, so disregarding
conformance differences of generic parameters in the type system is
fatal to soundness.  The general solution is fairly straightforward:
treat generic types as different if their arguments satisfy the
type's constraints using different conformances.

In Hylo, that also means a generic type with a concrete argument
cannot “escape” into a context where it would depend on different
conformances for that argument, or where the conformances the
argument depends on are not satisfied:

```hylo
// module A
public trait P { fun boo() -> Int }
public type X<T: P> { public memberwise init }

// module B
import A
private conformance Int: P {
  public fun boo() -> Int { return 3 }
}
let x = X<Int>()

// module C
import B
private conformance Int: P {
  public fun boo() -> Int { return 4 }
}
let x = X<Int>() // OK
let bx = B.x     // Error: X<Int> depends on a different conformance Int: P

// module C
import B

// Error: X<Int> depends on conformance Int: P, which is not in scope
let bx = B.x
```

- Note: the phrasing of the restriction is carefully chosen to handle
  cases like this, where some of the generic arguments are concrete:

    ```hylo
    // module A
    public trait P { fun boo() -> Int }
    public type X<T: P, U> { public memberwise init }

    // module B
    import A
    private conformance Int: P {
      public fun boo() -> Int { return 3 }
    }
    // Error: result T<Int, V> depends on a non-public 
    // conformance `Int: P`.
    public fun g<U>() -> X<Int, U>
    ```

## What It Means for Generic Programming

In Hylo, the only pattern where a more-specific implementation will
*always* be chosen over a more general one is when the specific
implementation is defined by a conformance that does not overlap any
others.  Any more-general implementations must be default
implementations of trait requirements (which are defined in trait
bodies).

### Programming Model

In this world, the generic programmer must think of specializations as
optimizations, with the expectation that they will only sometimes take
effect.  Program correctness, therefore, depends on specialized
implementations being *semantically substitutable* for
less-specialized implementations: they must uphold all the same
contracts (while it is *technically possible* to reason about the
meaning of programs that don't uphold this discipline, we don't
believe it is practical).

### What about “without loss of efficiency?”

According to Stepanov and McJones' *Elements Of Programming*,

> Generic Programming is an approach to programming that focuses on
> designing algorithms and data structures so that they work in the
> most general setting without loss of efficiency.

If a generic programmer cannot count on a specialized conformance
being chosen in all cases, abstraction comes with a performance
penalty.  Certainly, compared to C++, the language where Generic
Programming became a mature discipline, what we've described so far
fails to support a key element of the discipline.

We have considered ways to ensure the selection of more specialized
implementations in unoptimized code, but they all require programmers
to expose information about notional implementation details in the
signatures of generic functions so that more-specific witness tables
can be selected by the compiler.  This exposure of implementation
details would be viral, with the extra dispatch information from
callee signatures propagating into the signatures of their callers.

Fortunately, by enshrining the discipline of semantic
substitutability, we can offer ideal specialization selection as an
optimization. Uses of generics can be maximally monomorphized, and as
part of this process, the most-specific conformances possible can be
substituted.  That step makes Hylo competitive with C++.

Of course, monomorphization must stop at the edges of a resilience
domain, where there is an expectation that implementations can be
replaced without recompiling clients. Because C++ doesn't support that
kind of resilience for generics, that doesn't affect our comparison.

This optimization _does_ have implications for testing and debugging:
it becomes more important to test code with the optimization enabled,
and some people may prefer to enable it for regular development in
debug builds as well.
