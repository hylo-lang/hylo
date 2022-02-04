# Val

Val is an open source, general-purpose programming language designed around on the concept of _(mutable) value semantics_.
The language aims to be safe and efficient, yet expressive enough to support multiple programming paradigms and implement concurrent algorithms safely and efficiently.

Value semantics brings several advantages in terms of software correctness, performance, and maintainability.
In particular, it upholds local reasoning, allowing programmers (and compilers) to safely focus on confined sections of the program, without worrying about unintended side effects in unrelated components.

Val is heavily inspired by [Swift](https://swift.org) and [Rust](https://www.rust-lang.org), and 
it adopts many of their features.
Those include borrowing, higher-order functions, and relatively powerful support for generic programming (a.k.a. parametric polymorphism).

### (Mutable) Value Semantics

A type has value semantics if variables of that type have independent values: the value of a variable cannot change through operations on another variable.

```val
// Declares a generic 'Pair' type.
type Pair<T, U> { var fst: T; var snd: U }

// Creates a pair and call a function.
var p1 = Pair(fst: 4, snd: 2)
foo(p1)

// Always prints 'Pair(fst: 4, snd: 2)', regardless of what 'foo' does.
print(p1)
```

Because `Pair` has value semantics, `p1`'s value is guaranteed not to change through operations on `foo`'s argument.
This behavior is unlike most modern object-oriented languages (e.g., Java or Python) in which values of a compound type such as `Pair` share mutable state.

A type has _mutable_ value semantics if it supports part-wise, in-place mutation (i.e., without reassigning a variable of the type to a whole new value).

```val
p1.fst = 8    // Mutates the `fst` part of `p1` in-place.
print(p1.fst) // Prints '8'
```

To prevent assignments from creating shared mutable state, the right-hand side can either be copied or ["moved"](https://doc.rust-lang.org/rust-by-example/scope/move.html).
Since copying large data structures can be expensive, Val never copies implicitly; assigning a variable destructively moves it.

```val
var p2 = p1   // `p1` moves into `p2`
print(p2)     // Prints 'Pair(fst: 4, snd: 2)'
p1.fst = 0    // Type error!
```

Because sharing is only harmful in the presence of mutation, Val lets immutable values share their state.

```val
var p1 = Pair(fst: 4, snd: 2)
let p2 = p1
print(p2)     // Prints 'Pair(fst: 4, snd: 2)'
p1.fst = 0    // OK!
```

Here, `p2` is declared constant with `let`.
Hence, `p1` does not have to be moved as long as neither `p1` nor `p2` change.
In other words, `p1` temporarily shares its state with `p2`, until `p2`'s value is no longer used.

This behavior explains why calls to `print` did not cause any move in earlier examples: `print` is only reading the value so its argument can temporarily (i.e., for the duration of the call) share its state.

## Documentation

[The language guide](https://github.com/val-lang/val/wiki/Val's-Language-Guide) gives a tour of Val's most salient features, in a progressive fashion.
A more formal and authoritative documentation is on the way.

### Build Val

Val is distributed in the form of a Swift package, which can be built with [Swift Package Manager](https://www.swift.org/package-manager/).

Clone this repository and simply run `swift build -c release` to build the project.
This command will create an executable `.build/release/val`.
You can run that command with the flag `--help` to get a summary of its options.

### Run the Tests

Run `swift test` to execute all tests.

Most tests are actual Val programs annotated with comments that instruct the test runner of the expected results.
They are located in the directory `Tests/ValTests/TestCases`
