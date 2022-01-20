# Val

Val is an open source, general-purpose programming language designed around on the concept of _(mutable) value semantics_.
The language aims to be safe and efficient, yet expressive enough to support multiple programming paradigms and implement concurrent algorithms safely and efficiently.

Value semantics brings several advantages in terms of software correctness, performance, and maintainability.
In particular, it upholds local reasoning, allowing programmers (and compilers) to safely focus on confined sections of the program, without worrying about unintended side effects in unrelated components (a.k.a. spooky actions at a distance).

Val is heavily inspired by (and implemented in) [Swift](https://swift.org) and adopts many of its features, including higher-order functions, mutating methods, and relatively powerful support for generic programming (a.k.a. parametric polymorphism).

### (Mutable) Value Semantics

A type has value semantics if the value of a variable of this type can only be changed through operations on that variable.
A type has _mutable_ value semantics if it supports part-wise, in-place mutation.

```val
// Declares a generic 'Pair' type.
type Pair<T, U> { var fst: T; var snd: U }

// Creates two pairs.
var foo = Pair(fst: 4, snd: 2)
var bar = foo

// Mutates the second pair in-place (i.e., without new allocations).
bar.fst = 8

// Prints 4, not 8, because 'foo' and 'bar' are two independent values.
print(bar.fst)
```

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
