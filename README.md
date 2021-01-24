# Val

Val is a safe, concurrent programming language without any reference.

Val supports most features modern languages typically offer (overloading, first-class functions, types with methods, subtype polymorphism, etc.).
However, unlike many high-level languages, it does not rely on references to build abstractions.
Everything in Val is a value and adopts *value semantics*.
In other words, an object is defined only by its value, not its identity.
This means that

```
var a1 = [1, 2, 3]
var a2 = a1
a1.append(4)
print(a2)
// Prints "1, 2, 3"
```
