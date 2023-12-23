# Generic environments

A generic _signature_ is a set of generic parameter declarations and constraints thereupon.
For example, the generic signature of the following declaration contains one parameter declaration and two constraints.

```hylo
fun f<T: Collection where T.Element == Int>(x: X) {}
```

To make sense of a generic signature, the compiler creates a corresponding generic _environment_ gathering the information that it describes.
In essence, a generic environment is a set of axioms that can be used in conjunction with the type system to prove arbitrary formulae about type or value terms.
For example, given the generic signature above, the corresponding environment can be used to prove that `T.Element` has a member `infix+`.

> Note that trait decalrations also describe a generic environment.
> All traits define an implicit generic parameter denoting `Self` that is bound by the trait they declare.
> Constraints on associated type and values define additional axioms.

## Why are generic environments hard to build?

Building generic environments is a little tricky because generic signatures are expressed very declaratively.
Hence, the type checker must figure out the order in which it can read the environment's constraints to make sense of them.
Further, very little assumptions can be made about the kind of entity an expression should denote, as constraints can relate to both types and values.
For example, consider the following program:

```hylo
trait P { fun h() -> Bool }
fun g<x: T, T: P where x.h()>() { ... }
```

This program is perfectly valid.
`x` is declared as a generic value parameter of type `T`, which is known to conform to `P`.
This conformance lets us conclude that `h` is indeed a member function of `T` returning Booleans, so we're allowed to call `x.h`.

### Why isn't that the same as type checking anything else?

Name resolution typically relies on generic environments having been built to determine whether a generic type parameter conforms to a particular trait.
As a result, we can't reuse the general name resolution algorithm to answer questions about an environment being constructed.
Consider the following function:

```hylo
fun h<T: Collection, U: Collection where T.Element == U.Element>(_ x: T, _ y: U) {
  let a: T.Position = x.first_position()
  ...
}
```

To evaluate `T.Position`, name resolution finds the environment declaring `T`, asks for the traits to which `T` conforms, and then proceed with quialified name lookup in those traits to find an associated type or value declaration.
This strategy requires the environment to be built.
Therefore, if the construction of an environment is implemented as a single logical step, we can't evaluate `T.Element` in the above function because the fact that `T` conforms to `Collection` is not known yet.

This problem is compounded by the fact that we may have to deduce conformances through equality constraints.
For example, consider this setup:

```hylo
trait Q { type X }
type A<T: Collection, U: Q> { ... }
extension A where T == Array<U>, T.Element.X == Int { ... }
```

Resolving `T.Element.X` requires us to first conclude that `T.Element: Q`, which only holds because `T == Array<U>` implies that `T.Element == U` and `U: Q`.
It is a difficult problem because it requires us to perform some form of unification based not only on the shape of type terms but also on ad-hoc information about conformance.

### Why can't we simply reuse our solver?

One may think that a simple solution could be to assign a type variable to each generic parameter and simply reuse our constraint solver.
After all, we already have a way to solve member constraints that depend on unification to deal with generic argument deduction and overloading.
Sadly, name resolution isn't the only roadblock.

As tempting is it may be to throw all constraints at the solver, the problem here is that it is harder to establish facts about type variables.
To illustrate, consider the following example:

```hylo
public fun main() {
  fun id<T where T: Movable>(_ v: sink T) -> T { v }
  let x: Int = 1
  let y = id(x)
}
```

When we look at the expression `id(x)`, we can look at the declarations of all entities and use them to establish some "ground truth".
Here, we know that `id` is a function `[](sink T) -> T` and that `x` is an `Int`.
We can substitute `Int` for `T`, check that it is indeed `Movable`, and conclude that the expression has type `Int`.
Crucially, `T: Movable` only served as a _constraint_ to be checked the whole time.
We never had to infer anything from it because we could prove that `T: Movable` using some external evidence, specifically `Int: Movable`.
Contrast that with the following declaration

```hylo
fun foo<T where T: Collection, T.Element == Int>() { ... }
```

Here, `T: Collection` must be taken for granted, as it is the only way to make sense of `T.Element`.
Our constraint solver is not designed to do that because in general a conformance constraint must be checked, not assumed.
Otherwise, expressions like `id<Immovable()>` would pass type checking, as the solver would simply _assume_ `Immovable: Movable`.

So while it is possible that we could reuse (parts of) our solver, we would need to engineer a way to hold assumptions until we either have enough information to check them or can conclude that they must be taken for granted because they define some properties of a generic environment.
Note that whether or not we reuse our solver, that is the strategy we must implement anyway.

One minor complication to keep in mind is that there is no obvious way to assign blame in case an inconsistency is detected.
For example:

```hylo
fun bar<T where T: Collection, T == Bool>() { ... }
```

Here, `bar` has an inconsistent environment, assuming there is no user-defined conformance in scope making `Bool` a `Collection`.
Either of the two constraints can be blamed.
This problem is not specific to environment checking, though, as we also need a way to assign blame when infer the type of any other expression.

## Construction strategy

The biggest challenge is to figure out the order in which we process the expressions of a generic signature.
One way to tackle this issue is to assume that the order in which constraints are written is significant.
If we process expressions from left to right, then we can incrementally add facts to the typing contexts without having to consider what we don't know yet.
In other words, this strategy solves the problem of distinguishing between constraints that must be taken for granted and those that must be checked.

To illustrate, consider the following signature:

```hylo
<T, n: Int where T: Collection, T.Element == Int[n], T.Element.Element: Collection>
```

If we read this signature from left to right, we can conclude that:

1. `T` is a generic _type_ (by `T`)
2. `n` is a generic _value_ (by `n: Int`)
3. `T` conforms to `Collection` (by `T: Collection`)
4. `T.Element` is an associated type (by 3)
5. `Int[n]` is well-formed (by 2)
6. `T.Element` is `Int[n]` (by 4 and 5)

At this point we have read all declarations and constraints before `T.Element.Element: Collection` and can consider them granted.
We have yet to evaluate `T.Element.Element: Collection`, which at this point is a constraint we must _check_ w.r.t. the information we already gather.
If the check passed, then the implications of the constraints would be merged with our typing context.
Here, however, it doesn't because at this point we now that `T.Element.Element` is `Int` (by 6) and `Int: Collection` does not hold (assuming there is no user-defined conformance in scope).

There is one rule we must add to use this evaluation order: a generic parameters introduced without a bound are assumed type-kinded.
With this assumption, we never have to look at uses of a generic parameter to determine whether it is a type or a value.
For example, this signature is ill-formed: `<T, U where T == Int[U]>`.
Further, note that generic parameters can't be instantiated as traits, so if we see `T: U` we can always conclude that `T` must be a value and that `U` must be a tyoe.

The next challenge is to deduce the implications of a constraint, given a partially constructed environment.
Before we tackle this one, let's make a few observations:

- Since traits can't be declared in types, if the root of a name expression is a type we now that a constraint of the form `A: B` is an instance constraint.

- The LHS and/or RHS of an equality constraint must be rooted at a generic parameter.
  That means we do not have to infer `T == U` from `Array<T> == Array<U>`.
  However, we must accept constraints on parameters introduced by logically enclosing environments (e.g., `Element == Int` in an extension of `Array`).

### Determining the kind of generic parameters

Given the above observations, we can define a strategy to determine whether a generic parameter declaration denote a value or a type:

- If the parameter has no bound, it is a type.
- If the parameter has a bound, it is a type if and only if that bound is a name expression composed exclusively of nominal components that do not refer to any generic parameter.

For example, `T: A<X>` declares a value parameter, regardless of what name resolution can say about `A` or `X`.

### Determining constraints on associated types

When we process an equality constraint, we can look at the traits modeled by the generic parameters or associated types constrained to deduce additional information.
Consider the following generic signature:

```hylo
<T: Collection, U where T.Element == Int, U == Bool, T == Array<U>>
```

Once we have verified that `Array<U>` is well-formed, we should look at the traits to which `T` is known to conform.
Then, for each of these traits, we should look how `Array<U>` implements associated types and declarations to establish additional facts.
In this case, because `Array<U>::Collection.Element == U`, we can derive an additional constraint `T.Element == U` from `T == U`, which we must check.
Because at this point we'll have already concluded that `T.Element == Int` and `U == Bool`, we can conclude that the environment is inconsistent.
