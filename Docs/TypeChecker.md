# TypeChecker

This document gives a brief overview of Val's static type checker.
Its purpose is to verify that program sources satisfy Val's static type system, i.e. that all of written expressions have an appropriate type for the context in which they occur.
The main entry point for is the `TypeChecker` class, defined in `Sources/Sema/TypeChecker.swift`.

Although it is implemented as an AST pass, the type checker is actually a composition of three different relatively uncoupled components:
1. Constraint generation (`Sources/Sema/ConstraintGenerator.swift`)
2. Constraint solving (`Sources/Sema/ConstraintSolver.swift`)
3. Solution application (`Sources/Sema/TypeDispatcher.swift`)

## Constraint generation

This step consists of walking the AST to generate type constraints from expressions.

### Literals

Literal expressions are given a type variable which the solver will try to substitute for a type that conforms to `ExpressibleByBuiltin*Literal` (defined in the standard library).
During constraint solving, the solver uses a default conforming type for each literal as an escape hatch if surrounding context doesn't provide enough information to infer the conforming type.

## Constraint solving

This step consists of solving the generated equations, through type inference.

## Solution application

This pass consists of applying the solution computed by the previous pass to the AST.
