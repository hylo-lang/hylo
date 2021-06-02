# Val

Val aims to be a safe, concurrent, general-purpose programming language based on value semantics.

```val
// Declares a generic 'Pair' type.
type Pair<T, U> { var fst: T; var snd: U }

// Instantiates the type 'Pair', inferring the specialization arguments.
var foo = Pair(fst: 4, snd: 2)

// Copies the instance an mutates it.
var bar = foo
bar.fst = 8

// Prints 4, not 8, because 'foo' and 'bar' are two independent values.
print(bar.fst)
```

Val is heavily inspired by (and implemented in) [Swift](https://swift.org) and adopts many of its features, including higher-order functions, mutating methods, and a relatively powerful support for generic programming (a.k.a. parametric polymorphism).
