import AST
import Basic

/// A type constraint solver.
struct ConstraintSolver {

  init(
    system: ConstraintSystem,
    assumptions: SubstitutionTable,
    penalities: Int,
    bestScore: SolutionScore,
    context: AST.Context
  ) {
    self.system = system
    self.assumptions = assumptions
    self.penalities = penalities
    self.bestScore = bestScore
    self.context = context
  }

  /// The AST context.
  private let context: AST.Context

  /// The constraint system to solve.
  private var system: ConstraintSystem

  /// The assumptions of the type solver.
  private var assumptions: SubstitutionTable

  /// The current penalities of the solver's solution.
  private var penalities: Int

  /// The current set of errors the solver encountered.
  private var errors: [TypeError] = []

  /// The score of the best solution that was computed so far.
  private var bestScore: SolutionScore

  /// The current score of the solver's solution.
  private var currentScore: SolutionScore {
    return SolutionScore(penalities: penalities, errorCount: errors.count)
  }

  /// Solves the type constraint, or fails trying.
  mutating func solve() -> Solution {
    while let constraint = system.freshConstraints.popLast() {
      // Make sure the current solution is still worth exploring.
      guard currentScore <= bestScore else { break }

      // Attempt to solve the next constraint.
      switch constraint {
      case let c as EqualityCons    : solve(c)
      case let c as SubtypingCons   : solve(c)
      case let c as ConformanceCons : solve(c)
      case let c as ValueMemberCons : solve(c)
      case let c as DisjunctionCons : return solve(c)
      default:
        fatalError("unreachable")
      }
    }

    return Solution(assumptions: assumptions, penalities: penalities, errors: errors)
  }

  private mutating func solve(_ constraint: EqualityCons) {
    // Retrieves the current assumptions for both types.
    let lhs = assumptions[constraint.first]
    let rhs = assumptions[constraint.second]

    // If the types are obviously equivalent, we're done.
    if lhs == rhs { return }

    // Otherwise, attempt to unify the two types.
    switch (lhs, rhs) {
    case (let tau as TypeVar, _):
      assumptions.substitute(rhs, for: tau)
      system.refresh(constraintsDependingOn: tau)

    case (_, let tau as TypeVar):
      assumptions.substitute(lhs, for: tau)
      system.refresh(constraintsDependingOn: tau)

    case (let lhs as TupleType, let rhs as TupleType):
      checkTupleCompatibility(lhs, rhs, for: constraint)
      for (i, elems) in zip(lhs.elems, rhs.elems).enumerated() {
        system.insert(
          EqualityCons(elems.0.type, isEqualTo: elems.1.type,
                       at: constraint.locator?.appending(.typeTupleElem(i))))
      }

    case (let lhs as TupleType, _) where lhs.elems.count == 1:
      system.insert(
        EqualityCons(lhs.elems[0].type, isEqualTo: rhs, at: constraint.locator))

    case (_, let rhs as TupleType) where rhs.elems.count == 1:
      system.insert(
        EqualityCons(rhs.elems[0].type, isEqualTo: rhs, at: constraint.locator))

    case (let lhs as FunType, let rhs as FunType):
      // Break the constraint.
      system.insert(
        EqualityCons(lhs.paramType, isEqualTo: rhs.paramType,
                     at: constraint.locator?.appending(.parameter)))
      system.insert(
        EqualityCons(lhs.retType, isEqualTo: rhs.retType,
                     at: constraint.locator?.appending(.returnType)))

    default:
      errors.append(.conflictingTypes(constraint))
    }
  }

  private mutating func solve(_ constraint: SubtypingCons) {
    // Retrieves the current assumptions for both types.
    let lhs = assumptions[constraint.first]
    let rhs = assumptions[constraint.second]

    // If the types are obviously equivalent, we're done.
    if lhs == rhs { return }

    // Otherwise, attempt to solve the constraint.
    switch (lhs, rhs) {
    case is (TypeVar, TypeVar):
      // We can't solve anything yet if both types are still unknown.
      system.staleConstraints.append(constraint)

    case (_ as TypeVar, _):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // convertible to `U` and that are above `T`. Since we can't enumerate this set, we have to
      // make an educated guess about `T`.
      system.insert(EqualityCons(strengthening: constraint))

      // FIXME: Try to proceed with a stale version of the constraint to handle cases in which a
      // tigher constraint on `T` is discovered later.

    case (_, _ as TypeVar):
      // The type variable is above a more concrete type. We should compute the "join" of all types
      // to which `T` is convertible and that are below `U`.
      let alternatives: [Constraint] = [
        // FIXME: Add the views to which `T` conforms.
        EqualityCons(strengthening: constraint),
      ]
      system.insert(disjunction: alternatives)

    case (let lhs as FunType, let rhs as FunType):
      // Break the constraint.
      system.insert(
        EqualityCons(lhs.paramType, isEqualTo: rhs.paramType,
                     at: constraint.locator?.appending(.parameter)))

      // Codomains are contravariant!
      system.insert(
        SubtypingCons(rhs.retType, isSubtypeOf: lhs.retType,
                      at: constraint.locator?.appending(.returnType)))

    default:
      errors.append(.conflictingTypes(constraint))
    }
  }

  private mutating func solve(_ constraint: ConformanceCons) {
    let type = assumptions[constraint.type]
    let view = constraint.view

    switch type {
    case let tau as TypeVar:
      // Postpone the constraint if `T` is still unknown, unless `V` is a literal view. In this
      // case fall back to the associated default.
      if view.decl === context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral) {
        let defaultType = context.getTypeDecl(for: .Int)!.instanceType
        solve(EqualityCons(tau, isEqualTo: defaultType, at: constraint.locator))
      } else {
        system.staleConstraints.append(constraint)
      }

    case let nominal as NominalType:
      // Handle explicit and inherited view conformance.
      if nominal.decl.conformance(to: view) == nil {
        errors.append(.nonConformingType(constraint))
      }

    case is BuiltinIntType:
      if view.decl !== context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral) {
        errors.append(.nonConformingType(constraint))
      }

    default:
      // FIXME: Handle structural view conformance.
      solve(SubtypingCons(type, isSubtypeOf: view, at: constraint.locator))
    }
  }

  private mutating func solve(_ constraint: ValueMemberCons) {
    // We can't solve anything yet if `T` is still unknown.
    var baseType = assumptions[constraint.first]
    guard !(baseType is TypeVar) else {
      system.staleConstraints.append(constraint)
      return
    }

    // If `T` is an inout-type, then we should solve the constraint for its base.
    if let inoutType = baseType as? InoutType {
      baseType = inoutType.base
    }

    // FIXME: Handle tuple types.

    // Handle assignment operators on built-in types.
    if let builtinType = baseType as? BuiltinType,
       constraint.memberName == InfixOperator.copy.rawValue
    {
      solve(EqualityCons(
              context.getBuiltinAssignOperatorType(builtinType), isEqualTo: constraint.second,
              at: constraint.locator?.appending(.valueMember(constraint.memberName))))
      return
    }

    // The base should have a nominal type.
    guard let baseTypeDecl = (baseType as? NominalType)?.decl else {
      errors.append(.nonExistentProperty(constraint))
      return
    }

    // Retrieve the member's declaration.
    let decls = baseTypeDecl
      .lookup(constraint.memberName, in: context)
      .valueDecls
    guard !decls.isEmpty else {
      errors.append(.nonExistentProperty(constraint))
      return
    }

    var choices: [Constraint] = []
    for decl in decls {
      choices.append(
        EqualityCons(decl.type, isEqualTo: constraint.second,
                     at: constraint.locator?.appending(.valueMember(constraint.memberName))))
    }
    system.insert(disjunction: choices)
  }

  public mutating func solve(_ constraint: DisjunctionCons) -> Solution {
    precondition(!constraint.elements.isEmpty)

    var solutions: [Solution] = []
    for choice in constraint.elements {
      // Create a sub-solver for each choice in the disjunction.
      var subsolver = ConstraintSolver(
        system: system.fork(inserting: choice.constraint),
        assumptions: assumptions,
        penalities: penalities + choice.weight,
        bestScore: bestScore,
        context: context)
      let newSolution = subsolver.solve()

      // Discard inferior solutions
      if solutions.isEmpty || newSolution.score == bestScore {
        solutions.append(newSolution)
        bestScore = newSolution.score
      } else if newSolution.score < bestScore {
        solutions = [newSolution]
        bestScore = newSolution.score
      }
    }

    // Make sure there is only one solution.
    if solutions.count > 1 {
      errors.append(.ambiguousConstraint(constraint))
      // FIXME: What should we do to merge conflicting errors and assumptions?
    }

    return Solution(
      assumptions: solutions[0].assumptions,
      penalities: solutions[0].penalities,
      errors: errors + solutions[0].errors)
  }

  private mutating func checkTupleCompatibility(
    _ lhs: TupleType, _ rhs: TupleType, for constraint: Constraint
  ) {
    guard lhs.elems.count == rhs.elems.count else {
      errors.append(.conflictingTypes(constraint))
      return
    }

    for (lhs, rhs) in zip(lhs.elems, lhs.elems) {
      guard lhs.label == nil || rhs.label == nil || lhs.label == rhs.label else {
        errors.append(.conflictingLabels(constraint))
        return
      }
    }
  }

  /// The result of a solver.
  struct Solution {

    /// The type assumptions that were made to solve the constraint system.
    let assumptions: SubstitutionTable

    /// The penalities of the solution.
    let penalities: Int

    /// The errors associated with the solution.
    let errors: [TypeError]

    /// The score of the solution.
    var score: SolutionScore {
      return SolutionScore(penalities: penalities, errorCount: errors.count)
    }

  }

  /// The score of a solution.
  struct SolutionScore: RawRepresentable, Comparable {

    init(rawValue: UInt64) {
      self.rawValue = rawValue
    }

    init(penalities: Int, errorCount: Int) {
      rawValue =
        (UInt64(UInt32(truncatingIfNeeded: penalities))) |
        (UInt64(UInt32(truncatingIfNeeded: errorCount)) << 32)
    }

    let rawValue: UInt64

    static func < (lhs: SolutionScore, rhs: SolutionScore) -> Bool {
      return lhs.rawValue < rhs.rawValue
    }

    static var worst = SolutionScore(rawValue: UInt64.max)

  }

}
