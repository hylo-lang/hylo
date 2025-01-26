# Compiler Architecture

## Project Overview

The Hylo compiler is written in [Swift] and uses [LLVM] as its code generation
backend. It conforms to the the standard swift project layout:
* [Package.swift] the manifest file used by SPM
* [Sources] all the source code for the compiler
* [Tests] all the test code

Then there are some extra directories specific to the Hylo project:
* [Tools] Scripts to aid in development
* [Examples] Some real world Hylo programs
* [StandardLibrary] The Hylo standard (and core) library

## Stages of compilation

The Hylo compiler goes through the standard stages of compilation:

| Stage | Description | Important files |
| ----- | ----------- | --------------- |
| Tokenisation | Transforms Hylo source code (Strings) to a stream of distinct tokens | [Lexer.swift], [Token.swift] |
| Parsing | Creates an [abstract syntax tree] from the token stream | [Parser.swift] |
| Type-checking | Inspects the abstract syntax tree for type errors | [TypeChecking], [TypeChecker.swift] |
| IR-lowering | Generates the [intermediate representation] from the abstract syntax tree | [IR], [Emitter.swift] |
| LLVM IR generation | Convert Hylo IR into [LLVM] IR | [Transpilation.swift], [Swifty-LLVM] |
| Machine Code Generation | This is completely handled by [LLVM] | |

These top-level stages of the compiler are laid out in [Driver] where you
can see the outline of the compilation phases with their entry points.
Depending on the flags passed to the compiler, it can exit early at some of
these stages.

### Interesting parts

Most of the compiler does what you'd expect from the compiltion stages above
but some are worth a deeper look:

#### Abstract syntax tree

The abstract syntax tree is made up of an **append-only** array that produces
`NodeID` objects as indices into the array. The `NodeID` allows nodes to refer
to other nodes using their `NodeID`. `NodeID` is generic over node types and
allows us to constrain which nodes are allowed as leaves of other nodes.

The use of `NodeID` types as indices into an array allows us to define the
existence of a node, by its `NodeID`, without providing access to the node.
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

A successfully created `TypedProgram` means the Hylo program is well typed.

#### Hylo IR

The Hylo IR is composed of instructions defined in the [Instruction] module.
The [Emitter] is the component responsible for creating the `IR` and inserting it
into the [IR/Module], module-by-module and creating an [IR/Program].

The Hylo IR is only valid after it has gone through some mandatory passes
defined in `Module+*` files of [IR/Analysis]. After these passes the IR should
be valid and executable by a *theoretical* Hylo VM. Some [more passes] may be
necessary depending on the target.

## Important compiler modules

![PlantUML model](https://www.plantuml.com/plantuml/png/VL1TQiCm37xNANI1v01xA1qT6u4MHbRsxYLQOkBAHNOAPUpkmtYDxItqoVv-97tIrdabVZuPEAjkKWFXs2tV9z4N4bZmirZsn3r-0UADV7mxPkXLYQCC7wzdsetKJPg7FxG3mEZ7gjh4FeN_7XqRO90nmVZYRzbtIjwqPMPMvkgqowY3ui64a5dLplRqGboKxlVlX-0Pkp3cTg7uObrluaOFgFbY9tAvaY-nQcZQDNf9kog6F4c00B6gGY-0UDnQz33wV_DbPeSVz6c585UIlsbBrvQudq_c1m00)

[comment]: # (To generate this URL, copy the content of Modules.puml in http://www.plantuml.com/plantuml/uml/, and get the URL from there)

**Legend**:
| <b>Item</b> | <b>Meaning</b> |
| -- | -- |
| package | A folder in the source repository  |
| solid arrow | Dependency |
| dotted arrow | phase invocation / "runs before" relations |

**Packages**:
| Package | Description |
| -- | -- |
| [hc] | The actual compiler executable. Just calls [Driver].
| [Driver/] | Defines and executes the stages of the compiler. Takes care of compiler arguments. |
| [FrontEnd] | The frontend of the compiler. Handles lexing, parsing and type checking. Also stores the AST representation of the program. |
| [AST/] | The structures used to describe the abstract syntax tree of the input program. |
| [Parse] | The lexer and the parser for the Hylo source files. |
| [TypeChecking] | Implements the type checking of the Hylo programs. |
| [Types] | Defines the possible types that a Hylo entity might have. |
| [IR] | Defines the intermediate representation of Hylo programs, and the operarations associated with it. |
| [Analysis] | The analysis phases that can be run on the IR. |
| [Mangling] | The name-mangling algorithm used by Hylo. |
| [CodeGen] | Implements the code-generation phase for Hylo compiler. Currently only implements LLVM code generation. |
| [LLVM/] | The code generation into LLVM. |

[Swift]: https://en.wikipedia.org/wiki/Swift_(programming_language)
[LLVM]: https://en.wikipedia.org/wiki/LLVM
[SPM]: https://www.swift.org/package-manager/
[intermediate representation]: https://en.wikipedia.org/wiki/Intermediate_representation
[abstract syntax tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree

[Driver]: ../../Sources/Driver/Driver.swift
[Package.swift]: ../../Package.swift
[Sources]: ../../Sources
[Tests]: ../../Tests
[Tools]: ../../Tools
[Examples]: ../../Examples
[StandardLibrary]: ../../StandardLibrary
[AST]: ../../Sources/FrontEnd/AST/AST.swift
[ScopedProgram]: ../../Sources/FrontEnd/ScopedProgram.swift
[TypedProgram]: ../../Sources/FrontEnd/TypedProgram.swift
[Instruction]: ../../Sources/IR/Operands/Instruction/
[Emitter]: ../../Sources/IR/Emitter.swift
[IR/Module]: ../../Sources/IR/Module.swift
[IR/Program]: ../../Sources/IR/Program.swift
[IR/Analysis]: ../../Sources/IR/Analysis/
[more passes]: ../../Sources/IR/Analysis/Module+Depolymorphize.swift

[Lexer.swift]: ../../Sources/FrontEnd/Parse/Lexer.swift
[Token.swift]: ../../Sources/FrontEnd/Parse/Token.swift
[Parser.swift]: ../../Sources/FrontEnd/Parse/Parser.swift
[TypeChecking]: ../../Sources/FrontEnd/TypeChecking
[TypeChecker.swift]: ../../Sources/FrontEnd/TypeChecking/TypeChecker.swift
[IR]: ../../Sources/IR
[Emitter.swift]: ../../Sources/IR/Emitter.swift
[Transpilation.swift]: ../../Sources/CodeGen/LLVM/Transpilation.swift
[Swifty-LLVM]: https://github.com/hylo-lang/Swifty-LLVM
[hc]: ../../Sources/hc
[Driver/]: ../../Sources/Driver
[FrontEnd]: ../../Sources/FrontEnd
[AST/]: ../../Sources/FrontEnd/AST
[Parse]: ../../Sources/FrontEnd/Parse
[Types]: ../../Sources/FrontEnd/Types
[Analysis]: ../../Sources/IR/Analysis
[Mangling]: ../../Sources/IR/Mangling
[CodeGen]: ../../Sources/CodeGen
[LLVM/]: ../../Sources/CodeGen/LLVM

