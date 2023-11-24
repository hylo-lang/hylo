# Compiler Architecture

## Project Overview

The hylo compiler is written in [Swift] and uses [LLVM] as it's code generation
backend. It conforms to the the standard swift project layout:
* [Package.swift] the manifest file used by SPM
* [Sources] all the source code for the compiler
* [Tests] all the test code

Then there are some extra directories specific to the hylo project:
* [Tools] Scripts to aid in development
* [Examples] Some real world hylo programs
* [Library] The hylo standard (and core) library

## Stages of compilation

The hylo compiler goes through the standard stages of compilation:

1. Tokenisation: Transforms hylo source code (Strings) to stream of distinct
   tokens
1. Parsing: Creates an [abstract syntax tree] from the token stream
1. Type-checking: Inspects the abstract syntax tree for type errors
1. IR-lowering: Generates the [intermediate representation] from the abstract
   syntax tree
1. LLVM IR generation: Convert hylo IR into [LLVM] IR
1. Machine Code Generation: This is completely handled by [LLVM]

These top-level stages of the compiler are laid out in [Driver] where you
can see the outline of the compilation phases with their entry points.
Depending on the flags passed to the compiler, the compiler can exit early at
some of these stages.

### Interesting parts

Most of the compiler does what you'd expect from the compiltion stages above
but some are worth a deeper look:

#### Abstract syntax tree

The abstract syntax tree is made up of an **append-only** array that produces
`NodeID` objects as indices into the array. The `NodeID` allows nodes to refer
to other nodes using their `NodeID`. `NodeID` is generic over node types and
allows us to constrain which nodes are allowed as leaves of other nodes.

The use of `NodeID` types as indices into an array allows us to define the
existence of a node, by it's `NodeID`, without providing access to the node.
For access you still need the array. This is in contrast to traditional
references that provide existence AND access without allowing separation.

The use of `NodeID` types are ubiquitous and is often aliased to `.ID` of a
new type (e.g., `FunctionDecl.ID`).

#### Program Protocol

After the AST is created the compiler creates property maps that associate
properties to the nodes of the AST. Currently there are two distinct phases of
property creation for these property maps. The first is creating the
connections between scopes and nodes, stored in the `ScopedProgram` struct. The
second is where the majority of the type-checking happens, associating a type
for each expression, declaration etc. Each of these stages is composed of the
previous stage:

[AST] < [ScopedProgram] < [TypedProgram] < [IR/Program]

A successfully created `TypedProgram` means the hylo program is well typed.

#### Hylo IR

The Hylo IR is composed of instructions defined in the [Instruction] module.
The [Emitter] is the component responsible for creating the `IR` an inserting
it into the [IR/Module], module-by-module and creating an [IR/Program].

The hylo IR is only valid after it has gone through some mandatory passes
defined in `Module+*` files of [IR/Analysis]. After these passes the IR should
be valid and executable by a *theortical* hylo VM. Some [more passes] may be
necessary dependent on the target.

[Swift]: https://en.wikipedia.org/wiki/Swift_(programming_language)
[LLVM]: https://en.wikipedia.org/wiki/LLVM
[SPM]: https://www.swift.org/package-manager/
[intermediate representation]: https://en.wikipedia.org/wiki/Intermediate_representation
[abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree

[Driver]: ../Sources/Driver/Driver.swift
[Package.swift]: ../Package.swift
[Sources]: ../Sources
[Tests]: ../Tests
[Tools]: ../Tools
[Examples]: ../Examples
[Library]: ../StandardLibrary
[AST]: ../Sources/Core/AST/AST.swift
[ScopedProgram]: ../Sources/Core/ScopedProgram.swift
[TypedProgram]: ../Sources/FrontEnd/TypedProgram.swift
[Instruction]: ../Sources/IR/Operands/Instruction/
[Emitter]: ../Sources/IR/Emitter.swift
[IR/Module]: ../Sources/IR/Module.swift
[IR/Program]: ../Sources/IR/Program.swift
[IR/Analysis]: ../Sources/IR/Analysis/
[more passes]: ../Sources/IR/Analysis/Module+Depolymorphize.swift
