import AST
import Basic

/// A constraint system solver.
struct CSSolver {

  init(
    system          : ConstraintSystem,
    assumptions     : SubstitutionTable = SubstitutionTable(),
    overloadChoices : [ConstraintLocator: [ValueDecl]] = [:],
    penalities      : Int = 0,
    errors          : [TypeError] = [],
    bestScore       : Solution.Score = .worst,
    checker         : TypeChecker
  ) {
    self.system = system
    self.assumptions = assumptions
    self.overloadChoices = overloadChoices
    self.penalities = penalities
    self.errors = errors
    self.bestScore = bestScore
    self.checker = checker
  }

  /// The top-level type checker.
  private let checker: TypeChecker

  /// The constraint system to solve.
  private var system: ConstraintSystem

  /// The assumptions of the type solver.
  private var assumptions: SubstitutionTable

  /// The choice(s) that have been selected for each overload binding constraint solved.
  private var overloadChoices: [ConstraintLocator: [ValueDecl]]

  /// The current penalities of the solver's solution.
  private var penalities: Int

  /// The current set of errors the solver encountered.
  private var errors: [TypeError]

  /// The score of the best solution that was computed so far.
  private var bestScore: Solution.Score

  /// The current score of the solver's solution.
  private var currentScore: Solution.Score {
    return Solution.Score(penalities: penalities, errorCount: errors.count)
  }

  /// The AST context.
  private var context: AST.Context { checker.context }

  /// Solves the type constraint, or fails trying.
  mutating func solve() -> Solution {
    while let constraint = system.freshConstraints.popLast() {
      // Make sure the current solution is still worth exploring.
      guard currentScore <= bestScore else { break }

      // Attempt to solve the next constraint.
      switch constraint {
      case let c as RelationalConstraint      : solve(c)
      case let c as ValueMemberConstraint     : solve(c)
      case let c as OverloadBindingConstraint : return solve(c)
      case let c as DisjunctionConstraint     : return solve(c)
      default:
        fatalError("unreachable")
      }
    }

    // FIXME: Handle stale constraints.

    return Solution(
      bindings        : assumptions.flattened(),
      overloadChoices : overloadChoices,
      penalities      : penalities,
      errors          : errors)
  }

  private mutating func solve(_ constraint: RelationalConstraint) {
    // Retrieves the current assumptions for both types.
    let updated = RelationalConstraint(
      kind: constraint.kind,
      lhs: assumptions[constraint.lhs],
      rhs: assumptions[constraint.rhs],
      at: constraint.locator)

    // If the types are obviously equivalent, we're done.
    if updated.lhs == updated.rhs { return }

    switch updated.kind {
    case .equality      : solve(equality   : updated)
    case .conformance   : solve(conformance: updated)
    case .subtyping     : solve(subtyping  : updated)
    case .conversion    : solve(conversion : updated)
    }
  }

  private mutating func solve(equality constraint: RelationalConstraint) {
    // Attempt to unify the two types.
    switch (constraint.lhs, constraint.rhs) {
    case (let tau as TypeVar, _):
      assumptions.substitute(constraint.rhs, for: tau)
      system.refresh(constraintsDependingOn: tau)

    case (_, let tau as TypeVar):
      assumptions.substitute(constraint.lhs, for: tau)
      system.refresh(constraintsDependingOn: tau)

    case (let lhs as ExistentialType, _):
      if !lhs.genericEnv.equivalences.areEqual(lhs, constraint.rhs) {
        errors.append(.conflictingTypes(constraint))
      }

    case (_, let rhs as ExistentialType):
      if !rhs.genericEnv.equivalences.areEqual(constraint.lhs, rhs) {
        errors.append(.conflictingTypes(constraint))
      }

    case (let lhs as BoundGenericType, let rhs as BoundGenericType):
      guard lhs.decl === rhs.decl else {
        errors.append(.conflictingTypes(constraint))
        break
      }

      assert(lhs.args.count == rhs.args.count)
      for (larg, rarg) in zip(lhs.args, rhs.args) {
        system.insert(
          RelationalConstraint(kind: .equality, lhs: larg, rhs: rarg, at: constraint.locator))
      }

    default:
      // The types might be structural.
      if attemptStructuralMatch(constraint) {
        return
      }
      errors.append(.conflictingTypes(constraint))
    }
  }

  private mutating func solve(conformance constraint: RelationalConstraint) {
    let view = constraint.rhs as! ViewType

    switch constraint.lhs {
    case let tau as TypeVar:
      // Postpone the constraint if `T` is still unknown, unless `V` is a literal view. In this
      // case fall back to the associated default.
      if view.decl === context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral) {
        let defaultType = context.getTypeDecl(for: .Int)!.instanceType
        let simplified = RelationalConstraint(
          kind: .equality, lhs: tau, rhs: defaultType, at: constraint.locator)
        solve(simplified)
      } else {
        system.staleConstraints.append(constraint)
      }

    case let nominal as NominalType:
      // Handle explicit and inherited view conformance.
      if nominal.decl.conformance(to: view) == nil {
        errors.append(.nonConformingType(constraint))
      }

    case let existential as ExistentialType:
      if existential.genericEnv.conformance(of: existential, to: view) == nil {
        errors.append(.nonConformingType(constraint))
      }

    case is BuiltinIntType:
      if view.decl !== context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral) {
        errors.append(.nonConformingType(constraint))
      }

    default:
      // FIXME: Handle structural view conformance.
      errors.append(.conflictingTypes(constraint))
    }
  }

  private mutating func solve(subtyping constraint: RelationalConstraint) {
    // Otherwise, attempt to solve the constraint.
    switch (constraint.lhs, constraint.rhs) {
    case is (TypeVar, TypeVar):
      // We can't solve anything yet if both types are still unknown.
      system.staleConstraints.append(constraint)

    case is (TypeVar, ValType):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // coercible to `U` and that are above `T`. Since we can't enumerate this set, we have to
      // make an educated guess about `T`.
      let simplified = RelationalConstraint(
        kind: .equality, lhs: constraint.lhs, rhs: constraint.rhs, at: constraint.locator)
      solve(simplified)

      // FIXME: The above strategy will fail to handle cases where `T` is more trightly constrained
      // by another constraint that we haven't solved yet. One strategy to handle this case might
      // be to fork the system with a "strict subtyping" constraint. Should it succeed, it will get
      // a better score that the current solution.

    case is (ValType, TypeVar):
      // The type variable is above a more concrete type. We should compute the "join" of all types
      // to which `T` is coercible and that are below `U`.
      var guesses: [Constraint] = [
        RelationalConstraint(
          kind: .equality, lhs: constraint.lhs, rhs: constraint.rhs, at: constraint.locator)
      ]

      // If `T` is a nominal type, add all views to which it conforms to the set of guesses.
      if let nominal = constraint.lhs as? NominalType {
        // FIXME: Should we make sure we don't accidentally load conformances that come from a
        // a non-imported module if this is type-checked?
        guesses.append(contentsOf: nominal.decl.conformances.map({ conf in
          RelationalConstraint(
            kind: .equality, lhs: conf.viewDecl.instanceType, rhs: constraint.rhs,
            at: constraint.locator)
        }))
      }
      system.insert(disjunction: guesses)

    default:
      solve(equality: constraint)
    }
  }

  private mutating func solve(conversion constraint: RelationalConstraint) {
    switch constraint.rhs {
    case is BuiltinIntLiteralType:
      // A conversion constraint can always be solved as a conformance to the corresponding
      // `ExpressibleBy***` view.
      let literalView = context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!.instanceType
      let simplified = RelationalConstraint(
          kind: .conformance, lhs: constraint.lhs, rhs: literalView, at: constraint.locator)

      if constraint.lhs is TypeVar {
        // There are two cases two consider if `T` is unknown. One is that we simply didn't visit
        // the constraint(s) that will bind it yet; the other is that we don't have enough
        // information to infer it and should fall back to a default.
        let defaultType = context.getTypeDecl(for: .Int)!.instanceType
        let defaultChoice = RelationalConstraint(
          kind: .equality, lhs: constraint.lhs, rhs: defaultType, at: constraint.locator)
        system.insertDisjuncConf(disjunctionOfConstraintsWithWeights: [
          (defaultChoice, 0),
          (simplified   , 1),
        ])
      } else {
        solve(simplified)
      }

    default:
      fatalError("unreachable")
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
      let assignType = context.getBuiltinAssignOperatorType(builtinType)
      let simplified = RelationalConstraint(
        kind: .equality, lhs: assignType, rhs: constraint.rhs,
        at: constraint.locator.appending(.valueMember(constraint.memberName)))
      solve(simplified)
      return
    }

    // Retrieve the member's declaration(s).
    let decls = baseType.lookup(member: constraint.memberName).values
    guard !decls.isEmpty else {
      errors.append(.nonExistentProperty(constraint))
      return
    }

    if decls.count == 1 {
      // Only one choice; we can solve an equality constraint.
      if let varDecl = decls[0] as? VarDecl {
        checker.check(decl: varDecl.patternBindingDecl!)
      }

      let choiceType = decls[0].contextualize(
        from: constraint.useSite, processingContraintsWith: { prototype in
          system.insert(RelationalConstraint(prototype: prototype, at: constraint.locator))
        })
      let choice = RelationalConstraint(
        kind: .equality, lhs: choiceType, rhs: constraint.rhs, at: constraint.locator)
      solve(choice)

      // Save the "selected" overload for solution application.
      let locator = ConstraintLocator(constraint.locator.resolve())
      overloadChoices[locator] = [decls[0]]
    } else {
      // Several choices; we have to create an overload set.
      let simplified = OverloadBindingConstraint(
        constraint.rhs, declSet: decls, useSite: constraint.useSite, at: constraint.locator)
      system.insert(simplified)
    }
  }

  private mutating func solve(_ constraint: OverloadBindingConstraint) -> Solution {
    assert(!constraint.declSet.isEmpty)
    let type = assumptions[constraint.type]

    // Instanciate the type of the declaration candidates.
    let choices = constraint.declSet.map({ (decl) -> (Constraint, Int) in
      if let varDecl = decl as? VarDecl {
        checker.check(decl: varDecl.patternBindingDecl!)
      }

      let choiceType = decl.contextualize(
        from: constraint.useSite, processingContraintsWith: { prototype in
          system.insert(RelationalConstraint(prototype: prototype, at: constraint.locator))
        })
      let choice = RelationalConstraint(
        kind: .equality, lhs: type, rhs: choiceType, at: constraint.locator)
      return (choice, 0)
    })

    // Solve the set of choices as a disjunction of constraints.
    let results = branch(choices: choices)
    let declSet = results.map({ constraint.declSet[$0.index] })

    // If there's only one single best solution, we found our winner.
    let locator = ConstraintLocator(constraint.locator.resolve())
    if results.count == 1 {
      var solution = results[0].solution
      solution.overloadChoices[locator] = declSet
      return solution
    } else {
      overloadChoices[locator] = declSet
    }

    // The system is underspecified. Save the error and move on.
    errors.append(.multipleOverloads(constraint, declSet))
    return solve()
  }

  private mutating func solve(_ constraint: DisjunctionConstraint) -> Solution {
    precondition(!constraint.elements.isEmpty)
    let results = branch(choices: constraint.elements)
    var solution = results[0].solution

    // Make sure there is only one solution.
    if results.count > 1 {
      // FIXME: Refine the context of the ambiguity if we can guess its cause (e.g., a call to an
      // overloaded function).

      // We'll reach this point if there's no way to identify a unique best solution, most likely
      // because all candidates are equally invalid, hence we have no choice but to pick one based
      // on an arbitrary criterion (e.g., the first we discovered). Unfortunately, there is nothing
      // that guarantees this strategy to be deterministic.
      solution.errors.append(.ambiguousConstraint(constraint))
    }

    return solution
  }

  private mutating func branch(
    choices: [(Constraint, Int)]
  ) -> [(index: Int, solution: Solution)] {
    var results: [(index: Int, solution: Solution)] = []

    for i in 0 ..< choices.count {
      var subsolver = CSSolver(
        system          : system.fork(inserting: choices[i].0),
        assumptions     : assumptions,
        overloadChoices : overloadChoices,
        penalities      : penalities + choices[i].1,
        errors          : errors,
        bestScore       : bestScore,
        checker         : checker)
      let newSolution = subsolver.solve()

      // Discard inferior solutions
      if results.isEmpty || newSolution.score == bestScore {
        results.append((index: i, solution: newSolution))
        bestScore = newSolution.score
      } else if newSolution.score < bestScore {
        results = [(index: i, solution: newSolution)]
        bestScore = newSolution.score
      }
    }

    return results
  }

  private mutating func attemptStructuralMatch(_ constraint: RelationalConstraint) -> Bool {
    switch (constraint.lhs, constraint.rhs) {
    case (let lhs as TupleType, let rhs as TupleType):
      // Check if the layouts match.
      checkTupleCompatibility(lhs, rhs, for: constraint)

      // Break down the constraint.
      for i in 0 ..< min(lhs.elems.count, rhs.elems.count) {
        system.insert(
          RelationalConstraint(
            kind: constraint.kind, lhs: lhs.elems[i].type, rhs: rhs.elems[i].type,
            at: constraint.locator.appending(.typeTupleElem(i))))
      }
      return true

    case (let lhs as TupleType, _) where lhs.elems.count == 1:
      if lhs.elems[0].label != nil {
        errors.append(.conflictingLabels(constraint))
      }
      system.insert(
        RelationalConstraint(
          kind: constraint.kind, lhs: lhs.elems[0].type, rhs: constraint.rhs,
          at: constraint.locator))
      return true

    case (_, let rhs as TupleType) where rhs.elems.count == 1:
      if rhs.elems[0].label != nil {
        errors.append(.conflictingLabels(constraint))
      }
      system.insert(
        RelationalConstraint(
          kind: constraint.kind, lhs: constraint.lhs, rhs: rhs.elems[0].type,
          at: constraint.locator))
      return true

    case (let lhs as FunType, let rhs as FunType):
      if constraint.kind == .subtyping {
        // Notice that we swap `lhs.paramType` with `rhs.paramType`, so as to account for the
        // contravariance of function parameters if the constraint has subtyping semantics.
        system.insert(
          RelationalConstraint(
            kind: constraint.kind, lhs: rhs.paramType, rhs: lhs.paramType,
            at: constraint.locator.appending(.parameter)))
      } else {
        system.insert(
          RelationalConstraint(
            kind: constraint.kind, lhs: lhs.paramType, rhs: rhs.paramType,
            at: constraint.locator.appending(.parameter)))
      }

      system.insert(
        RelationalConstraint(
          kind: constraint.kind, lhs: lhs.retType, rhs: rhs.retType,
          at: constraint.locator.appending(.returnType)))
      return true

    default:
      return false
    }
  }

  private mutating func checkTupleCompatibility(
    _ lhs: TupleType, _ rhs: TupleType, for constraint: RelationalConstraint
  ) {
    // The tuples don't match unless they have the same number of elements.
    guard lhs.elems.count == rhs.elems.count else {
      errors.append(.conflictingTypes(constraint))
      return
    }

    // The tuples don't match unless they have the same labels.
    for (lhs, rhs) in zip(lhs.elems, lhs.elems) where lhs.label != rhs.label {
      errors.append(.conflictingLabels(constraint))
      return
    }
  }

}
