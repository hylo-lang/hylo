# Definite (De)Initialization

Definite (De)Initialization (DI, a.k.a. [definite assignment analysis](https://en.wikipedia.org/wiki/Definite_assignment_analysis) is a mandatory transformation pass that is applied on Hylo's intermediate representation, before code generation.
This pass ensures that all objects are initialized before use (definite initialization) and deinitialized before the end of their storage's lifetime (definite deinitialization), inserting additional instructions if necessary.

The pass is expected to run early in the IR transformation pipeline, after __Implicit Return Insertion__ and __Unreachable Code Elimination__.

DI guarantees that objects are treated as linear resources.
Specifically, it ensures that any object assigned to a register is consumed exactly once and that, given a memory location `l`:
- loading `l` or starting a `let` or `inout` on `l` cannot occur unless the object at `l` is fully initialized; and
- deallocating `l` or starting a `set` borrow on `l` cannot occur unless the referred storage is uninitialized or the object that occupied it has been fully consumed.

DI fails with a diagnostic if it cannot guarantee these properties.

## Implementation

DI is implemented as an abstract interpreter that models the contents of the call stack and local registers.
The interpreter is intraprocedural: it runs one function at a time, as the signature of a function is sufficient to fully capture the effects of a call on the abstract domain.

### Abstract domain

The abstract domain of the interpreter is composed of abstract memory locations and object initialization states.

An abstract memory location is a *root* location and a (possibly empty) path of abstract offsets identifying a sub-location.
A *root* location is either:
- a constant value; or
- a `let`, `inout` or `set` parameter; or
- the result of an instruction.

Example:
```
%0 = alloc_stack T // results in (%0.0, [])
%1 = borrow [set] %0, 0, 1 // results in (%0.0, [0, 1])
```

*Note: Root locations do not store the empty array in the actual implementation.*

An initialization state is either a tag representing the state of an object or a non-empty table mapping each abstract offset of an object to the initialization state of its part.
The tags denote either:
- a fully initialized object; or
- a fully uninitialized object; or
- a fully consumed object, along with the set of instructions that may have consumed it.

An initialization state is *canonical* if it's a tag or if it's a table mapping at least two offsets to different values.
For example, the canonical form of `[0: initialized, 1: initialized]` is simply `initialized`.

The evaluation context of the interpreter is composed of two tables, called `locals` and `memory`.
`locals` maps function parameters and instruction results to either initialization states or sets of memory locations.
`memory` maps memory locations to initialization states.

### Execution

The execution of a function is a fixed-point search that terminates when these two conditions are met:
1. all basic blocks have been visited at least once; and
2. visiting any basic block won't result in a different after-context than the one already computed.

One step of the algorithm consists of selecting a block, computing its before-context from the after-contexts of its predecessors, and computing its after-context by executing its instructions.

#### Block selection

The algorithm always starts by visiting the function's entry, which is never the successor of another block.
The before-context of that basic block is determined from the function's parameters.
Subsequent blocks are pulled from a work list, represented as a FILO queue, and initialized non-deterministically with all the reachable basic blocks of the function.

A block can't be visited before its [immediate dominator](https://en.wikipedia.org/wiki/Dominator_(graph_theory)) and the predecessors that it does not dominate have been visited at least once.
That restriction guarantees that all locals and memory locations required for a block's evaluation exist in its before-context, and that the analysis does not detect false positive.
Blocks that cannot be visited are put back at the end of the queue, guaranteeing that their dominators will eventually be visited before the next attempt.

For example, consider the following function, which encodes a very simple loop:

```
@lowered fun foo(let &Bool) -> Void {
bb0(%0 : &Bool):
 %1 = alloc_stack Bool, binding="y"
 %2 = borrow [let] %0
 %3 = call [let, let] @Hylo.Bool.Copy, %2
 %4 = borrow [set] %1
 store %3, %4
 branch bb1
bb1():
 %5 = borrow [let] %1, 0
 cond_branch %5, bb2, bb3
bb2():
 %6 = record Bool, i1(0x0)
 %7 = borrow [set] %1
 store %6, %7
 branch bb1
bb3():
 dealloc_stack %1
 return void
}
```

The execution starts with `bb0`, after deriving its before-context from `foo`'s parameters.
All instructions are executed to compute `bb0`'s after-state, which is saved.

Next, the interpreter picks `bb1` as the next block, as neither `bb2` nor `bb3` cannot be visited yet (note: `bb1` dominates both).
The before-context of `bb1` is computed by merging the after-contexts of all visited predecessors.
Since `bb2` has not been visited yet, only `bb0` is considered.

Next, the interpreter can pick either `bb2` or `bb3`.
In both cases, the before-contexts are the after-context of `bb1`.

Terminating condition (1) is met once the after-contexts of `bb2` and `bb3` have been.
However, condition (2) isn't because the after-context of `bb2` changed after `bb1` was visited, meaning that a second visit may lead to a different result.
Therefore, the interpreter re-visits `bb1`, this time merging the after-contexts of `bb0` and `bb2` to form a new before-context.

Finally, terminating condition (2) is met because the second visit does not change the after-context of `bb1`.

#### Interpretation

The instructions of a block are interpreted sequentially to update the context of the interpreter.
Instruction results are stored in the `locals` table while side effects on the memory are reflected on the `memory` table.

Instruction results with an object type are assigned to fully initialized objects while instruction results with an address type are assigned to sets of abstract memory locations.
Sets are being used as a conservative approximation of the actual memory location produced by the instruction during concrete execution.

To illustrate, consider the following statements, which create a projection:

```hylo
var pair = (4, 2)
inout x = &min[&pair.0, &pair.1]
```

At the IR level, these two statements are roughly lowered as follows:

```
%0 = alloc_stack
...
%5 = borrow [inout] %0, 0
%6 = borrow [inout] %0, 0
%7, %8 = start_subscript [inout] [let, inout, inout] @min, %5, %6
```

`%7` denotes the projection created by the subscript while `%8` is the subscript continuation.
Statically, `%7` is assumed to be either `(%0.0, [0])` or `(%0.0, [1])`.

A valid program maintains the invariant that if a register is assigned to a set of locations `L`, then all objects at that location have the same initialization state.

*Note: The invariant does not hold when uniqueness is violated.*
*DI will not catch such violation, but __Ownership__ will.*

#### Context merging

Context merging occurs to compute the before-context of a basic block with multiple predecessors.
The operation consists of creating a conservative approximation of all the possible states at the beginning of the basic block.
Such an approximation is obtained by forming unions of memory locations assigned to the same register and eagerly deinitializing objects that are not initialized after all predecessors.

To illustrate, consider the following function, which conditionally initializes a local variable:

```
@lowered fun foo(let &Bool) -> Int {
bb0(%0 : &Bool):
 %1 = alloc_stack Int, binding="x"
 %2 = borrow [let] %0, 0
 cond_branch %2, bb1, bb2:
bb1():
 %3 = record Bool, i64(0x2a)
 %4 = borrow [set] %0
 store %3, %4
 branch bb2
bb2():
 %5 = load %1
 dealloc_stack %1
 return %5
}
```

The variable `x` is initialized if and only if the argument to the function is `true`.
Therefore, DI should catch the attempt to return the value of `x` in `bb2`.

When the before-context of `bb2` is computed, the interpreter observes that `(%0.0, [])` is initialized in the after-context of `bb1`, but not in that of `bb0`.
In response, it inserts the deinitialization of that object before `bb1` branches and considers the location uninitialized in the before-context of `bb2`.
Therefore, when `%5 = load %1` is executed, the interpreter properly diagnoses that `x` is not definitely initialized.
