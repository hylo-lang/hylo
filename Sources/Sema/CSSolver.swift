import AST
import Basic

/// A constraint system solver.
struct CSSolver {

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
      case let c as EqualityConstraint    : solve(c)
      case let c as SubtypingConstraint   : solve(c)
      case let c as ConversionConstraint  : solve(c)
      case let c as ConformanceConstraint : solve(c)
      case let c as ValueMemberConstraint : solve(c)
      case let c as DisjunctionConstraint : return solve(c)
      default:
        fatalError("unreachable")
      }
    }

    return Solution(assumptions: assumptions, penalities: penalities, errors: errors)
  }

  private mutating func solve(_ constraint: EqualityConstraint) {
    // Retrieves the current assumptions for both types.
    let lhs = assumptions[constraint.lhs]
    let rhs = assumptions[constraint.rhs]

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
          EqualityConstraint(elems.0.type, isEqualTo: elems.1.type,
                             at: constraint.locator?.appending(.typeTupleElem(i))))
      }

    case (let lhs as TupleType, _) where lhs.elems.count == 1:
      system.insert(
        EqualityConstraint(lhs.elems[0].type, isEqualTo: rhs, at: constraint.locator))

    case (_, let rhs as TupleType) where rhs.elems.count == 1:
      system.insert(
        EqualityConstraint(rhs.elems[0].type, isEqualTo: rhs, at: constraint.locator))

    case (let lhs as FunType, let rhs as FunType):
      // Break the constraint.
      system.insert(
        EqualityConstraint(lhs.paramType, isEqualTo: rhs.paramType,
                           at: constraint.locator?.appending(.parameter)))
      system.insert(
        EqualityConstraint(lhs.retType, isEqualTo: rhs.retType,
                           at: constraint.locator?.appending(.returnType)))

    default:
      errors.append(.conflictingTypes(constraint))
    }
  }

  private mutating func solve(_ constraint: SubtypingConstraint) {
    // Retrieves the current assumptions for both types.
    let lhs = assumptions[constraint.lhs]
    let rhs = assumptions[constraint.rhs]

    // If the types are obviously equivalent, we're done.
    if lhs == rhs { return }

    // Otherwise, attempt to solve the constraint.
    switch (lhs, rhs) {
    case is (TypeVar, TypeVar):
      // We can't solve anything yet if both types are still unknown.
      system.staleConstraints.append(constraint)

    case is (TypeVar, ValType):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // coercible to `U` and that are above `T`. Since we can't enumerate this set, we have to
      // make an educated guess about `T`.
      solve(EqualityConstraint(lhs, isEqualTo: rhs, at: constraint.locator))

      // FIXME: The above strategy will fail to handle cases where `T` is more trightly constrained
      // by another constraint that we haven't solved yet. One strategy to handle this case might
      // be to fork the system with a "strict subtyping" constraint. Should it succeed, it will get
      // a better score that the current solution.

    case is (ValType, TypeVar):
      // The type variable is above a more concrete type. We should compute the "join" of all types
      // to which `T` is coercible and that are below `U`.
      var guesses: [Constraint] = [EqualityConstraint(lhs, isEqualTo: rhs, at: constraint.locator)]

      // If `T` is a nominal type, add all views to which it conforms to the set of guesses.
      if let nominal = lhs as? NominalType {
        // FIXME: Should we make sure we don't accidentally load conformances that come from a
        // a non-imported module if this is type-checked?
        guesses.append(contentsOf: nominal.decl.conformances.map({ conf in
          EqualityConstraint(conf.viewDecl.instanceType, isEqualTo: rhs, at: constraint.locator)
        }))
      }
      system.insert(disjunction: guesses)

    case (let lhs as FunType, let rhs as FunType):
      // Break the constraint. Parameters are contravariants, return types are covariants.
      system.insert(
        SubtypingConstraint(rhs.paramType, isSubtypeOf: lhs.paramType,
                           at: constraint.locator?.appending(.parameter)))
      system.insert(
        SubtypingConstraint(lhs.retType, isSubtypeOf: rhs.retType,
                            at: constraint.locator?.appending(.returnType)))

    case is (BuiltinIntLiteralType, ValType):
      // A constraint of the form `BuiltinIntLiteralType ≤ T` might be solved by conversion.
      solve(ConversionConstraint(rhs, convertsTo: lhs, at: constraint.locator))

    default:
      errors.append(.conflictingTypes(constraint))
    }
  }

  private mutating func solve(_ constraint: ConversionConstraint) {
    // Retrieves the current assumptions for both types.
    let lhs = assumptions[constraint.lhs]
    let rhs = assumptions[constraint.rhs]

    // If the types are obviously equivalent, we're done.
    if lhs == rhs { return }

    switch rhs {
    case is TypeVar:
      // We can't solve anything yet if the result of the conversionis still unknown.
      system.staleConstraints.append(constraint)

    case is BuiltinIntLiteralType:
      precondition(context.stdlib != nil, "standard library is not loaded")
      let view = context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!.instanceType
      solve(ConformanceConstraint(lhs, conformsTo: view as! ViewType, at: constraint.locator))

    default:
      solve(SubtypingConstraint(lhs, isSubtypeOf: rhs, at: constraint.locator))
    }
  }

  private mutating func solve(_ constraint: ConformanceConstraint) {
    let type = assumptions[constraint.type]
    let view = constraint.view

    // If the types are obviously equivalent, we're done.
    if type == view { return }

    switch type {
    case let tau as TypeVar:
      // Postpone the constraint if `T` is still unknown, unless `V` is a literal view. In this
      // case fall back to the associated default.
      if view.decl === context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral) {
        let defaultType = context.getTypeDecl(for: .Int)!.instanceType
        solve(EqualityConstraint(tau, isEqualTo: defaultType, at: constraint.locator))
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
      solve(SubtypingConstraint(type, isSubtypeOf: view, at: constraint.locator))
    }
  }

  private mutating func solve(_ constraint: ValueMemberConstraint) {
    // We can't solve anything yet if `T` is still unknown.
    var baseType = assumptions[constraint.lhs]
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
      solve(EqualityConstraint(
              context.getBuiltinAssignOperatorType(builtinType), isEqualTo: constraint.rhs,
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
      .lookup(unqualified: constraint.memberName, in: context)
      .values
    guard !decls.isEmpty else {
      errors.append(.nonExistentProperty(constraint))
      return
    }

    var choices: [Constraint] = []
    for decl in decls {
      choices.append(
        EqualityConstraint(decl.type, isEqualTo: constraint.rhs,
                           at: constraint.locator?.appending(.valueMember(constraint.memberName))))
    }
    system.insert(disjunction: choices)
  }

  public mutating func solve(_ constraint: DisjunctionConstraint) -> Solution {
    precondition(!constraint.elements.isEmpty)

    var solutions: [Solution] = []
    for choice in constraint.elements {
      // Create a sub-solver for each choice in the disjunction.
      var subsolver = CSSolver(
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
    _ lhs: TupleType, _ rhs: TupleType, for constraint: RelationalConstraint
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
