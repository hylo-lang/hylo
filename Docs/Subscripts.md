# Notes about subscript implementation

A subscript is lowered as a single Hylo IR function.
This function has no return value but is expected to execute exactly one `yield` instruction on every possible path from entry to each `return` instruction.

At the call site, a subscript is invoked with the `project` instruction.
The lifetime pass closes a projection with `end_project` after its last use.

**Example:**

Consider the following Hylo code:

```swift
subscript sum(_ x: Int, _ y: Int): Int { x + y }

fun foo(a: Int, b: Int) {
  let x = sum(a, b)
  print(x)
}
```

Its lowered form is:

```
subscript sum(let Int, let Int): Int {
b0(%0 : &Int, %1 : %Int):
  %2 = alloc_stack Int
  %3 = borrow [set] %2
  %4 = borrow [let] %1
  %5 = borrow [let] %0
  %6 = call @Int.infix+, %5, %4
  end_borrow %5
  end_borrow %4
  store %6, %3
  end_borrow %3
  %7 = borrow [let] %2
  yield %7
  end_borrow %7
  %8 = load %2
  deinit %8
  dealloc_stack %2
  return void
}

fun foo(let Int, let Int) -> Void {
b0(%0 : &Int, %1 : %Int):
  %2 = borrow [let] %0
  %3 = borrow [let] %1
  %4 = project @sum, %2, %3
  %5 = borrow [let] %4
  %6 = call @print, %5
  end_borrow %5
  end_project %4
  end_borrow %3
  end_borrow %2
  return void
}
```

## Checking the 1-`yield` rule

Since `yield` cannot appear in a loop, we don't need to use dominance.
For each basic block containing a `yield`, we can simply check if the set of successor blocks includes one basic block (possibly the same) that contains a `yield`.

## Compilation

There are two approaches to compile subscripts.
The first, which we'll call *subscripts-as-continuations*, is to compile a "ramp" function corresponding to the slice of the code up to a `yield` instruction and `n` "slide" functions corresponding the the slices after each `yield` instruction.

The caller initiates a projection by calling the ramp function and passing it a lambda representing at least the code using the projection.
This continuation is called with another lambda denoting the tail corresponding to the `yield` instruction executed by the ramp.
It is expected to call that lambda when it is done with the projection.

*Note: this approach is related to  [yield-once returned-continuation lowering](https://llvm.org/docs/Coroutines.html#returned-continuation-lowering).*

The second approach, which we'll call *subscripts-as-sessions*, also compiles a ramp and its slides, but the ramp returns the yielded value rather than calling a continuation with it.
To avoid dynamic allocation, the ramp accepts a pointer to a pre-allocated representing its stack, which the caller can push on its stack before the call.

The term "session" captures the intuition that the caller initiates a session by calling a ramp and terminates it by calling a slide.
The advantage of this approach is that it requires involves code transformation.

### Using continuations

One important challenge to implement subscripts-as-continuations is to identity the scope of continuations.
Specifically, the continuation passed from the caller to a ramp can only contain instructions that are dominated by the start of the projection and must terminate all projections that it starts.

For example, consider the following Hylo program:

```swift
public fun main() {
  var s = [1, 2, 3, 4]
  while condition() {
    let t = s[0]
    let u = t
    print(t)
    print(u)
  }
  print(s)
}
```

In lowered form, this program has the following shape:

```
fun main() -> Void {
b0:
  ...
  branch loop.head
loop.head:
  ...
  cond_branch %b, loop.body, loop.tail
loop.body:
  ...
  %t = project @Buffer<Int>.[], %x0, %x1
  ...
  %u = project @Buffer<Int>.[], %x2, %x3
  ...
  end_project %t
  ...
  end_project %u
  ...
  branch loop.head
loop.tail:
  ...
  %x4 = call @print, %s
}
```

The continuation passed to the ramp starting `t`'s projection must contain at least `end_project %u`, because `u`'s projection starts within the lifetime of `t`.
But it cannot contain `%x4 = call @print, %s` because `%x4` is not dominated by `%t`.

These two restrictions serve two goals:
1. Guarantee that continuations cannot break control flow and cannot.
2. Handle overlapping projections that do not nest.

One over-approximation of a continuation's scope is the dominance frontier of the basic block in which the projection starts, excluding the instructions before the start of the projection.
In the above example, the continuation associated with `t` would contain all instructions in `loop.body` after the definition of `%t`.

Using dominance frontiers also obviates the difficulty to identify definitions may escape a continuation.
Those represent intermediate results computed within the lifetime of the projection and used after.
For example, in the following sequence, `%2` escapes the useful lifetime of `%1`:

```
%1 = project ...
%2 = alloc_stack ...
end_project %1
%3 = load %2
```

Still, intermediate results that are passed as block arguments beyond across a dominance frontier must be extracted out of the continuation.
Those must be stored in the continuation's argument and read by the ramp's caller to which this continuation was passed.

### Using sessions

With subscripts-as-sessions, one issue is that the caller must determine how much memory it must allocate for the ramp.
To maintain ABI stability, this information cannot be part of the ramp's signature, as it would imply that any change of a subscript's *implementation* may modify its ABI signature.

One simple workaround is to define an additional helper function that returns the size and alignment of the ramp's frame.
However, an inherant limitation of this approach is that it restricts ramps to use fixed-size frames, thereby prohibiting the use of `alloca` and similar features.

### Constructing frames

With both subscripts-as-continuations and subscripts-as-sessions, one must be able to determine the offsets of each local variable relative to the base of a stack frame in a target-independent manner.
One approach is to allocate a whole stack frame a value whose layout is known in advance.

It is likely that this strategy will hamper general-purpose optimizations.
In particular, it will probably disable [memory to register promotion](https://llvm.org/docs/Passes.html#mem2reg-promote-memory-to-register).

*See also: [Stack maps](https://llvm.org/docs/StackMaps.html) may be of help.*

## Addressors

An addressor is a subscript that does not synthesize the value that it projects and instead exposes a stored part of one of its arguments.
Such a subscript does not need any slide because it can simply compute the address of the projected value in its ramp and cleanup its stack before returning it.
