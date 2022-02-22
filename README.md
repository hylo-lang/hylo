# Val

Val is an open source, general-purpose programming language designed around on the concept of _(mutable) value semantics_.
The language aims to be safe and efficient, yet expressive enough to support multiple programming paradigms and implement concurrent algorithms safely and efficiently.

Value semantics brings several advantages in terms of software correctness, performance, and maintainability.
In particular, it upholds local reasoning, allowing programmers (and compilers) to safely focus on confined sections of the program, without worrying about unintended side effects in unrelated components.

Val is heavily inspired by [Swift](https://swift.org) and [Rust](https://www.rust-lang.org), and it adopts many of their features, including higher-order functions, powerful support for generic programming (a.k.a. parametric polymorphism), and an ownership-aware typesystem.
Qualitatively, Val aims to combine the systems programming power of Rust with the simplicity of Swift's programming model.
 
## (Mutable) Value Semantics

A type has **value semantics** if variables of that type have independent values: the value of a variable cannot change through operations on another variable.

```val
// Declares a generic 'Pair' type.
type Pair<T, U> = (fst: T, snd: U)

// Creates a pair and call a function.
var p1 = Pair(fst: 4, snd: 2)
foo(p1)

// Always prints '(fst: 4, snd: 2)', regardless of what 'foo' does.
print(p1)
```

Because `Pair` has value semantics, `p1`'s value is guaranteed not to change through operations on `foo`'s argument.
This behavior is unlike most modern object-oriented languages (e.g., Java or Python) in which values of a compound type such as `Pair` share mutable state.

Because immutable types have value semantics trivially, we say a type has **mutable value semantics** if it has value semantics _and_ supports in-place mutation of its parts (i.e., without reassigning a variable of the type to a whole new value).

```val
p1.fst = 8    // Mutates the `fst` part of `p1` in-place.
print(p1.fst) // Prints '8'.
```

## Variables

To prevent variable initializations and assignments from creating shared mutable state, the right-hand side can either be copied or ["moved"](https://doc.rust-lang.org/rust-by-example/scope/move.html).
Since copying large data structures can be expensive, all copies in Val are made explicitly.
Thus, variable initialization and assignment destructively move the source value (which itself may be an explicit copy).

```val
var p2 = p1        // `p1` is moved into `p2`.
var p3 = p2.copy() // `p2` is copied into `p3`.
p3.snd += 1
print(p2)          // Prints '(fst: 4, snd: 2)'.
print(p3)          // Prints '(fst: 4, snd: 3)'.
print(p1)          // Error: lifetime of p1 ended when it was moved into p2.
```

## Pass-by-value

`print` is a function taking one argument by value:

```val
// Emits a textual description of x to the console.
fun print<T>(_ x: T) { ... }
```

A parameter passed by value is immutable in the callee and borrowed from the caller.
Therefore, the caller need never make a copy in order to call `print`.
Val's strict value semantics statically guarantees that the callee cannot observe any change to its parameter during the call. 

## Immutable variables

Immutable variables (`let` bindings) operate on the same principle as by-value parameters: they share storage with their source.
However, unlike in a function call, the source of a `let` binding remains in scope during the lifetime of the binding.
Instead of borrowing the source, a `let` binding confers immutability on its source for the duration of the binding.

```val
var p1 = Pair(fst: 4, snd: 2)
let p2 = p1   // p1 and p2 share storage.
// p1.fst = 1 // Error: p1 is immutably bound to p2.
print(p2)     // Prints '(fst: 4, snd: 2)'.
p1.fst = 0    // OK: p2's lifetime has ended.
```

Note that variable lifetimes end at their last (lexical) use, allowing mutation to resume at the earliest possible point.
Mutatation of `p1` while `p2` is still in use would have required a copy:

```val
var p1 = Pair(fst: 4, snd: 2)
let p2 = p1.copy()
p1.fst = 1         // OK: p2 has its own storage.
print(p2)          // Prints '(fst: 4, snd: 2)'.
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
