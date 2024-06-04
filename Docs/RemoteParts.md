# 28/07/22 Session on slices and remote parts:

A remote part (previously stored projection) is a property that is stored remotely (unless it's `remote sink`).

Arguments to remote parameters in an initializer form lifetime bounds.
Example:

```
type S {
  var part: remote inout Int
}

public fun main(){
  var i = 0
  let s = S(part: i) // `i` is let-bound by `s`
  i = 9
  print(s)
}
```

The compiler should emit a warning if a remote part is taken as a parameter of an initializer but never stored in the object.
The compiler should emit a warning if a remote part is taken as a parameter of any other method.

Local `var` bindings operate differently than stored `var` properties.
When we assign into a local `var`, the value that it held previously is destroyed (unless we do the `copy(into:)` optimization).
When we assign into a stored `var`, the value of its container is modified in place.
The theoretical rationale is that a dotted access is always a projection: the LHS of `foo.bar = 2` is not actually a `var`.
Example:

```
public fun main(){
  var fruits = Array(["durian"])
  fruits = Array([]) // value held by fruits is destroyed

  type T {
    var fruits = ["durian"]
  }
  var x = T()
  x.fruits = [] // value held my `x` is mutated in place
}
```

A type may have polymorphic effects.
We have different categories of effects, which include "access" effects (let, inout, sink, set).
Those correspond to the different categories of subscript accessors.
For example, the slice below is polymorphic over a type of collection and the access (or mutability) of that collection.

```
type Slice<Base: Collection, base_access: access = sink> {
  var base: remote base_access Base
  var region: Range<Base.Index>
}
```

*Note: `remote` and `sink` "cancel out": `remote sink T` is equivalent to `T`.*

Conformance and extension declarations can depend on effects:

```
conformance Slice: Movable where base_access == sink {}
```

The output type of a subscript can be declared with `var`, in which case the yielded value can be mutable even when it is projected out of an immutable argument.

```
extension Array {

  subscript (in range: Range<Int>) : var Slice<Self, yielded> {
    let {
      var s = Slice(self, range)
      yield &s
      print(s)
    }
    inout { &Slice(self, range) }
  }

}
```

*Note: It's okay to write `var s = Slice(self, range)` because initialization is not consuming: it's binding a value.*
*Assigning a remote part can never change the referred part; it's modifying the yielded value anyway.*
*Translate to C++: References can't be reseated.*

```
public fun main() {

  let array = [1,2,2]
  inout part = &array[0] // This part is projected inout; type error because `array` is immutbale
  
  var slice = array[in: 1 ..< 10] // Okay, even if `array` is immutable, because of the var on the output type of the projection

  print(array)

}
```
