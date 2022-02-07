import AST
import Basic

/// A constraint system solver.
struct CSSolver {

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

  /// The AST context.
  private var context: AST.Context

  /// The current score of the solver's solution.
  private var currentScore: Solution.Score {
    return Solution.Score(penalities: penalities, errorCount: errors.count)
  }

  init(
    system: ConstraintSystem,
    assumptions: SubstitutionTable = SubstitutionTable(),
    overloadChoices: [ConstraintLocator: [ValueDecl]] = [:],
    penalities: Int = 0,
    errors: [TypeError] = [],
    bestScore: Solution.Score = .worst,
    context: AST.Context
  ) {
    self.system = system
    self.assumptions = assumptions
    self.overloadChoices = overloadChoices
    self.penalities = penalities
    self.errors = errors
    self.bestScore = bestScore
    self.context = context
  }

  /// Returns a copy of this solver.
  private func fork(system: ConstraintSystem, penalities: Int) -> CSSolver {
    return CSSolver(
      system: system,
      assumptions: assumptions,
      overloadChoices: overloadChoices,
      penalities: penalities,
      errors: errors,
      bestScore: bestScore,
      context: context)
  }

  /// Builds a solution with the current state of the solver.
  private func makeSolution() -> Solution {
    var solution = Solution(
      bindings: assumptions.flattened(),
      overloadChoices: overloadChoices,
      penalities: penalities,
      errors: errors)

    if !system.freshConstraints.isEmpty || !system.staleConstraints.isEmpty {
      solution.errors.append(
        .unsolvedConstraints(system.freshConstraints + system.staleConstraints))
    }

    return solution
  }

  /// Solves the type constraint, or fails trying.
  mutating func solve() -> Solution {
    // Process fresh constraints.
    while let constraint = system.freshConstraints.popLast() {
      // Make sure the current solution is still worth exploring.
      guard currentScore <= bestScore else { return makeSolution() }

      // Attempt to solve the next constraint.
      switch constraint {
      case let c as RelationalConstraint:
        solve(c)
      case let c as ValueMemberConstraint:
        solve(c)
      case let c as TupleMemberConstraint:
        solve(c)
      case let c as OverloadBindingConstraint:
        return solve(c)
      case let c as DisjunctionConstraint:
        return solve(c)
      default:
        fatalError("unreachable")
      }
    }

    // Attempt to refresh stale subtyping constraints.
    if !system.staleConstraints.isEmpty {

      // Implementation note: constraints of the form `T <: τ` or `τ <: T` become stale when the
      // solver can't find a way to constrain `τ` more tightly. Picking `T` as a substitution for
      // `τ` may break the stalemate.
      //
      // We iteratively substitute each stale subtyping constraint into a fresh equality constraint
      // and try to solve the system in a sub-solver. That process is guaranteed to terminate as
      // each sub-solver gets a smaller set of stale constraints.

      var solutions = SolutionSet<()>()
      for i in 0 ..< system.staleConstraints.count {
        let constraint = system.staleConstraints.removeLast()
        defer {
          system.staleConstraints.append(constraint)
          system.staleConstraints.swapAt(i, system.staleConstraints.count - 1)
        }

        if var constraint = constraint as? RelationalConstraint {
          guard
            constraint.kind == .subtyping ||
            constraint.kind == .paramSubtyping && (
              constraint.lhs is FunParamType || constraint.rhs is FunParamType)
          else {
            continue
          }

          constraint.kind = .equality
          var subsolver = fork(
            system: system.fork(inserting: constraint),
            penalities: penalities + 1)
          let newSolution = subsolver.solve()
          bestScore = min(bestScore, solutions.insert(((), newSolution)))
        }
      }

      // Make sure there is only one solution.
      if let (_, best) = solutions.best {
        return best
      }
    }

    // Return the best solution we found.
    return makeSolution()
  }

  private mutating func solve(_ constraint: RelationalConstraint) {
    // Retrieves the current assumptions for both types.
    var updated = constraint
    updated.lhs = assumptions[constraint.lhs]
    updated.rhs = assumptions[constraint.rhs]

    // If the types are obviously equivalent, we're done.
    if updated.lhs == updated.rhs { return }

    switch updated.kind {
    case .equality, .oneWayEquality:
      solve(equality: updated)
    case .conformance:
      solve(conformance: updated)
    case .subtyping, .paramSubtyping:
      solve(subtyping: updated)
    case .conversion:
      solve(conversion: updated)
    }
  }

  /// Solves an equality constraint.
  private mutating func solve(equality constraint: RelationalConstraint) {
    // Attempt to unify the two types.
    switch (constraint.lhs, constraint.rhs) {
    case (_, let tau as TypeVar):
      // Check if we're allowed to unify the right operand.
      guard constraint.kind != .oneWayEquality else {
        system.staleConstraints.append(constraint)
        return
      }

      assumptions.substitute(constraint.lhs, for: tau)
      system.refresh(constraintsDependingOn: tau)

    case (let tau as TypeVar, _):
      assumptions.substitute(constraint.rhs, for: tau)
      system.refresh(constraintsDependingOn: tau)

    case (let lhs as SkolemType, _):
      if !lhs.genericEnv.equivalences.areEqual(lhs, constraint.rhs) {
        errors.append(.conflictingTypes(constraint))
      }

    case (_, let rhs as SkolemType):
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
        system.insert(RelationalConstraint(
          kind: .equality, lhs: larg, rhs: rarg, at: constraint.locator))
      }

    case (let lhs as UnionType, let rhs as UnionType):
      guard lhs.canonical === rhs.canonical else {
        errors.append(.conflictingTypes(constraint))
        break
      }

    default:
      // Attempt to solve the constraint after desugaring the types.
      if attemptSolveDesugared(constraint) { return }

      // Attempt to simplify the constraint if the types match structurally.
      if attemptStructuralMatch(constraint) { return }

      // The constraint failed.
      errors.append(.conflictingTypes(constraint))
    }
  }

  /// Solves a view conformance constraint.
  private mutating func solve(conformance constraint: RelationalConstraint) {
    let view = constraint.rhs as! ViewType

    switch constraint.lhs {
    case let lhs as TypeVar:
      // Postpone the constraint if LHS is still unknown, unless `V` is a literal view. In this
      // case fall back to the associated default.
      if view.decl === context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral) {
        let defaultType = context.getTypeDecl(for: .Int)!.instanceType
        let simplified = RelationalConstraint(
          kind: .equality, lhs: lhs, rhs: defaultType, at: constraint.locator)
        solve(simplified)
      } else {
        system.staleConstraints.append(constraint)
      }

    case let lhs as NominalType:
      // Handle explicit and inherited view conformance.
      if lhs.decl.conformanceTable[view] == nil {
        errors.append(.nonConformingType(constraint))
      }

    case let lhs as SkolemType:
      if lhs.genericEnv.conformance(of: lhs.interface, to: view) == nil {
        errors.append(.nonConformingType(constraint))
      }

    case let lhs as AssocType:
      switch assumptions[lhs.base] {
      case let base as NominalType:
        // The base has been resolved; we can resolve the member.
        guard let witness = base.decl.witness(for: lhs.interface.decl) else {
          errors.append(.nonConformingType(constraint))
          break
        }

        // If the base of the associated type is a bound generic type, check if the referred
        // declaration is a type argument.
        let member: ValType
        if let base = base as? BoundGenericType,
           let param = witness.instanceType as? GenericParamType,
           let arg = base.bindings[param]
        {
          member = arg
        } else {
          member = witness.instanceType.canonical
        }

        let simplified = RelationalConstraint(
          kind: .conformance, lhs: member, rhs: view, at: constraint.locator)
        solve(simplified)

      case is TypeVar:
        // The base has yet to be resolved.
        system.staleConstraints.append(constraint)

      default:
        errors.append(.nonConformingType(constraint))
      }

    default:
      // FIXME: Handle structural view conformance.
      errors.append(.nonConformingType(constraint))
    }
  }

  /// Solves a subtyping constraint.
  private mutating func solve(subtyping constraint: RelationalConstraint) {
    switch (constraint.lhs, constraint.rhs) {
    case is (TypeVar, TypeVar):
      // We can't solve anything yet if both types are still unknown.
      system.staleConstraints.append(constraint)

    case is (TypeVar, ValType):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // coercible to RHS and that are above LHS. Unfortunately, we can't enumerate such a set; it
      // would essentially boils down to computing the set of types that are subtypes of RHS. Thus,
      // we have to make an educated guess, based on RHS.
      switch constraint.rhs {
      case is ProductType, is SkolemType:
        // RHS is a grounded type that cannot have any subtype.
        let simplified = RelationalConstraint(
          kind: .equality, lhs: constraint.lhs, rhs: constraint.rhs, at: constraint.locator)
        solve(simplified)

      case is ViewType:
        // RHS is a view type that to which any type could conform. Hence, binding LHS to RHS might
        // fail if LHS is more tightly constrained by another relation that we haven't solved yet.
        // Instead, we can try to solve the constraint as a conformance relation.
        let simplified = RelationalConstraint(
          kind: .conformance, lhs: constraint.lhs, rhs: constraint.rhs, at: constraint.locator)
        solve(simplified)

      default:
        // FIXME: Handle structural subtyping.
        system.staleConstraints.append(constraint)
      }

    case is (ValType, TypeVar):
      // The type variable is above a more concrete type. We should compute the "join" of all types
      // to which LHS is coercible and that are below RHS. Unfortunately, that set is unbounded
      // because of union types. Instead, we should postpone the constraint until we can get more
      // precise information on RHS.
      system.staleConstraints.append(constraint)
      return

    case (let lhs as FunParamType, let rhs as FunParamType):
      // Both LHS and RHS are parameter types. `LHS <: RHS` requires both to have the same passing
      // policy or that RHS be consuming.
      if (lhs.policy != rhs.policy) && (rhs.policy != .consuming) {
        errors.append(.conflictingPolicies(constraint))
        return
      }

      // Solve the constraint between the raw types. If either LHS or RHS is mutating, both must
      // have the same type. Otherwise, subtyping is fine.
      var simplified = RelationalConstraint(
        kind: .subtyping, lhs: lhs.rawType, rhs: rhs.rawType, at: constraint.locator)
      if (lhs.policy == .inout) || (rhs.policy == .inout) {
        simplified.kind = .equality
      }
      solve(simplified)

    case (_, let rhs as FunParamType):
      // RHS is a parameter type. If RHS is mutating, then LHS must be the same as RHS.
      var simplified = RelationalConstraint(
        kind: .subtyping, lhs: constraint.lhs, rhs: rhs.rawType, at: constraint.locator)
      if rhs.policy == .inout {
        simplified.kind = .equality
      }
      solve(simplified)

    case (let lhs as FunParamType, _):
      // LHS is a parameter type. We can solve the constraint w.r.t. its raw type.
      let simplified = RelationalConstraint(
        kind: .subtyping, lhs: lhs.rawType, rhs: constraint.rhs, at: constraint.locator)
      solve(simplified)

    case (_, let rhs as ViewType):
      // RHS is a view, to which LHS should conform.
      let simplified = RelationalConstraint(
        kind: .conformance, lhs: constraint.lhs, rhs: rhs, at: constraint.locator)
      solve(simplified)

    case (_, let rhs as ViewCompositionType):
      // Complain if the left operand is asynchronous.
      if constraint.lhs is AsyncType {
        errors.append(.nonSubtype(constraint))
        return
      }

      // All types trivially conform to any.
      if rhs.views.isEmpty { return }

      // Break the constraint into conformance relations for each of the views in the composition.
      for view in rhs.views {
        system.insert(RelationalConstraint(
          kind: .conformance, lhs: constraint.lhs, rhs: view, at: constraint.locator))
      }

    case (let lhs as UnionType, let rhs as UnionType):
      // Both LHS and RHS are union types: every element in LHS must be a subtype of RHS.
      for elem in lhs.elems {
        system.insert(RelationalConstraint(
          kind: constraint.kind, lhs: elem, rhs: rhs, at: constraint.locator))
      }

    case (_, let rhs as UnionType):
      // LHS is (partially) determined and is not a union type. Therefore, it must be a subtype of
      // at least one element in RHS.
      let choices = rhs.elems.map({ elem in
        RelationalConstraint(
          kind: constraint.kind, lhs: constraint.lhs, rhs: elem, at: constraint.locator)
      })
      system.insert(disjunction: choices)

    case (let lhs as AsyncType, let rhs as AsyncType):
      // LHS and RHS are both asynchronous. Simplify with their respective underlying type.
      let simplified = RelationalConstraint(
        kind: constraint.kind, lhs: lhs.base, rhs: rhs.base, at: constraint.locator)
      solve(subtyping: simplified)

    case (_, let rhs as AsyncType):
      // FIXME: Is that correct?
      // LHS is (partially) determined and is not an asynchronous type. Therefore, it must be
      // a subtype of RHS's underlying type.
      let simplified = RelationalConstraint(
        kind: constraint.kind, lhs: constraint.lhs, rhs: rhs.base, at: constraint.locator)
      solve(simplified)

    case (let lhs as KindType, let rhs as KindType):
      // LHS and RHS are both kind types. Simplify with their respective instance type.
      let simplified = RelationalConstraint(
        kind: constraint.kind, lhs: lhs.type, rhs: rhs.type, at: constraint.locator)
      solve(simplified)

    default:
      // Attempt to solve the constraint after desugaring the types.
      if attemptSolveDesugared(constraint) { return }

      // The types might be unifiable.
      solve(equality: constraint)
    }
  }

  /// Solves a conversion constraint.
  private mutating func solve(conversion constraint: RelationalConstraint) {
    switch constraint.rhs {
    case is BuiltinIntLiteralType:
      // A conversion constraint can always be solved as a conformance to the corresponding
      // `ExpressibleBy***` view.
      let literalView = context.getTypeDecl(for: .ExpressibleByBuiltinIntLiteral)!.instanceType
      let simplified = RelationalConstraint(
          kind: .conformance, lhs: constraint.lhs, rhs: literalView, at: constraint.locator)

      if constraint.lhs is TypeVar {
        // There are two cases two consider if LHS is unknown. One is that we simply didn't visit
        // the constraint(s) that will bind it yet; the other is that we don't have enough
        // information to infer it and should fall back to a default.
        let defaultType = context.getTypeDecl(for: .Int)!.instanceType
        let defaultChoice = RelationalConstraint(
          kind: .equality, lhs: constraint.lhs, rhs: defaultType, at: constraint.locator)
        system.insert(disjunctionOfConstraintsWithWeights: [
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

  /// Solves a value member constraint.
  private mutating func solve(_ constraint: ValueMemberConstraint) {
    // We can't solve anything if LHS is still unknown.
    var baseType = assumptions[constraint.lhs]
    if baseType is TypeVar {
      system.staleConstraints.append(constraint)
      return
    }

    // If LHS is a parameter type, solve the constraint for its raw type.
    if let paramType = baseType as? FunParamType {
      baseType = paramType.rawType
    }

    // If LHS is a tuple type, we try to match the specified member name with a label.
    if let tupleType = baseType as? TupleType {
      guard let elem = tupleType.elems.first(where: { $0.label == constraint.memberName }) else {
        errors.append(.nonExistentProperty(constraint))
        return
      }

      // Solve an equality constraint.
      let simplified = RelationalConstraint(
        kind: .equality, lhs: elem.type, rhs: constraint.rhs, at: constraint.locator)
      solve(simplified)
      return
    }

    // LHS must be a nominal type, or an existential type.
    var args: [GenericParamType: ValType] = [:]
    let selfDecls = baseType.lookup(member: "Self").types
    if !selfDecls.isEmpty {
      assert(selfDecls.count == 1)
      let existentialSelf = selfDecls[0].instanceType as! GenericParamType
      args[existentialSelf] = baseType
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
        _ = TypeChecker.check(decl: varDecl.patternBindingDecl!)
      }

      if let boundType = baseType as? BoundGenericType,
         let env = boundType.decl.prepareGenericEnv()
      {
        args.merge(
          zip(env.params, boundType.args),
          uniquingKeysWith: { (_, _) in fatalError("unreachable") })
      }

      let choiceType = TypeChecker.contextualize(
        decl: decls[0],
        from: constraint.useSite,
        args: args,
        processingContraintsWith: { system.insert(prototype: $0, at: constraint.locator) })
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

  /// Solves a tuple member constraint.
  private mutating func solve(_ constraint: TupleMemberConstraint) {
    // We can't solve anything yet if LHS is still unknown.
    var baseType = assumptions[constraint.lhs]
    if baseType is TypeVar {
      system.staleConstraints.append(constraint)
      return
    }

    // If LHS is a parameter type, solve the constraint for its raw type.
    if let paramType = baseType as? FunParamType {
      baseType = paramType.rawType
    }

    // The constraint obviously fails if LHS is not a tuple-type.
    guard let tupleType = baseType as? TupleType else {
      errors.append(.nonExistentProperty(constraint))
      return
    }

    // Make sure the tuple has enough members.
    guard tupleType.elems.count > constraint.memberIndex else {
      errors.append(.nonExistentProperty(constraint))
      return
    }

    // Simplify the constraint.
    let simplified = RelationalConstraint(
      kind: .equality, lhs: tupleType.elems[constraint.memberIndex].type, rhs: constraint.rhs,
      at: constraint.locator)
    solve(simplified)
  }

  /// Solves an overloaded constraint.
  private mutating func solve(_ constraint: OverloadBindingConstraint) -> Solution {
    assert(!constraint.declSet.isEmpty)
    let type = assumptions[constraint.type]

    // Instanciate the type of the declaration candidates.
    let choices = constraint.declSet.compactMap({ (decl) -> DisjunctionConstraint.Element? in
      // If the candidate is a variable, we may have to type-check the pattern binding declaration
      // to infer the variable's type from its initializer.
      if let decl = decl as? VarDecl {
        guard TypeChecker.check(decl: decl.patternBindingDecl!) else { return nil }
      }

      // Contextualize the declaration's type in case it is generic.
      let choiceType = TypeChecker.contextualize(
        decl: decl,
        from: constraint.useSite,
        processingContraintsWith: { system.insert(prototype: $0, at: constraint.locator) })

      let choice = RelationalConstraint(
        kind: .equality, lhs: type, rhs: choiceType, at: constraint.locator)
      return (choice, 0)
    })

    // Bail out if all available overloads failed type checking.
    if choices.isEmpty {
      errors.append(.noViableOverload(constraint))
      return makeSolution()
    }

    // Solve the set of choices as a disjunction of constraints.
    let solutions = branch(choices: choices)
    let declSet = solutions.map({ constraint.declSet[$0.id] })

    // If there's only one single best solution, we found a winner.
    let locator = ConstraintLocator(constraint.locator.resolve())
    if var (_, best) = solutions.best {
      best.overloadChoices[locator] = declSet
      return best
    } else {
      overloadChoices[locator] = declSet
    }

    // The system is underspecified.
    errors.append(.multipleOverloads(constraint, declSet))
    return makeSolution()
  }

  /// Solves a disjunction constraint.
  private mutating func solve(_ constraint: DisjunctionConstraint) -> Solution {
    precondition(!constraint.elements.isEmpty)
    let solutions = branch(choices: constraint.elements)

    guard let (_, best) = solutions.best else {
      // We couldn't find a single best solution. Let's use what we were able to infer so far.
      if !solutions.isEmpty {
        errors.append(.ambiguousConstraint(constraint))
      }
      return makeSolution()
    }

    return best
  }

  private mutating func branch(
    choices: [DisjunctionConstraint.Element]
  ) -> SolutionSet<Int> {

    // Solve each branch in a sub-solver.
    var solutions = SolutionSet<Int>()
    for i in 0 ..< choices.count {
      var subsolver = fork(
        system: system.fork(inserting: choices[i].constraint),
        penalities: penalities + choices[i].weight)
      let newSolution = subsolver.solve()
      bestScore = min(bestScore, solutions.insert((i, newSolution)))
    }

    // Clear all remaining constraints, as they must have been handled by each sub-solvers.
    system.freshConstraints.removeAll()
    system.staleConstraints.removeAll()
    return solutions
  }

  private mutating func attemptSolveDesugared(_ constraint: RelationalConstraint) -> Bool {
    switch (constraint.lhs, constraint.rhs) {
    case (let lhs as AliasType, _):
      let aliasedType = (lhs.decl as! AliasTypeDecl).realizeAliasedType()
      let simplified = RelationalConstraint(
        kind: constraint.kind, lhs: aliasedType, rhs: constraint.rhs, at: constraint.locator)
      solve(simplified)
      return true

    case (_, let rhs as AliasType):
      let aliasedType = (rhs.decl as! AliasTypeDecl).realizeAliasedType()
      let simplified = RelationalConstraint(
        kind: constraint.kind, lhs: constraint.lhs, rhs: aliasedType, at: constraint.locator)
      solve(simplified)
      return true

    case (let lhs as BoundGenericType, _) where lhs.decl is AliasTypeDecl:
      let aliasedDecl = lhs.decl as! AliasTypeDecl
      let aliasedType = aliasedDecl.realizeAliasedType().specialized(with: lhs.bindings)
      let simplified = RelationalConstraint(
        kind: constraint.kind, lhs: aliasedType, rhs: constraint.rhs, at: constraint.locator)
      solve(simplified)
      return true

    case (_, let rhs as BoundGenericType) where rhs.decl is AliasTypeDecl:
      let aliasedDecl = rhs.decl as! AliasTypeDecl
      let aliasedType = aliasedDecl.realizeAliasedType().specialized(with: rhs.bindings)
      let simplified = RelationalConstraint(
        kind: constraint.kind, lhs: constraint.lhs, rhs: aliasedType, at: constraint.locator)
      solve(simplified)
      return true

    default:
      return false
    }
  }

  private mutating func attemptStructuralMatch(_ constraint: RelationalConstraint) -> Bool {
    assert(constraint.isStructural)
    switch (constraint.lhs, constraint.rhs) {
    case (let lhs as TupleType, let rhs as TupleType):
      // Number of elements should match.
      if lhs.elems.count != rhs.elems.count {
        errors.append(.conflictingTypes(constraint))
      }

      // Break down the constraint.
      for i in 0 ..< min(lhs.elems.count, rhs.elems.count) {
        let (a, b) = (lhs.elems[i], rhs.elems[i])

        // Labels should match.
        if a.label != b.label { errors.append(.conflictingLabels(constraint)) }

        // Elements are covariant.
        system.insert(RelationalConstraint(
          kind: constraint.kind, lhs: a.type, rhs: b.type,
          at: constraint.locator.appending(.typeTupleElem(i))))
      }
      return true

    case (let lhs as TupleType, _) where lhs.elems.count == 1:
      if lhs.elems[0].label != nil {
        errors.append(.conflictingLabels(constraint))
      }
      system.insert(RelationalConstraint(
        kind: constraint.kind, lhs: lhs.elems[0].type, rhs: constraint.rhs,
        at: constraint.locator))
      return true

    case (_, let rhs as TupleType) where rhs.elems.count == 1:
      if rhs.elems[0].label != nil {
        errors.append(.conflictingLabels(constraint))
      }
      system.insert(RelationalConstraint(
        kind: constraint.kind, lhs: constraint.lhs, rhs: rhs.elems[0].type,
        at: constraint.locator))
      return true

    case (let lhs as FunType, let rhs as FunType):
      // Number of parameters should match.
      if lhs.params.count != rhs.params.count {
        errors.append(.conflictingTypes(constraint))
      }

      // Break down the constraint.
      let paramConstraintKind: RelationalConstraint.Kind = constraint.kind == .subtyping
        ? .paramSubtyping
        : .equality

      for i in 0 ..< min(lhs.params.count, rhs.params.count) {
        var (a, b) = (lhs.params[i], rhs.params[i])

        // Labels should match.
        if a.label != b.label { errors.append(.conflictingLabels(constraint)) }

        // Parameters are contravariant.
        if constraint.kind == .subtyping { swap(&a, &b) }
        system.insert(RelationalConstraint(
          kind: paramConstraintKind, lhs: a.type, rhs: b.type,
          at: constraint.locator.appending(.parameter(i))))
      }

      system.insert(RelationalConstraint(
        kind: constraint.kind, lhs: lhs.retType, rhs: rhs.retType,
        at: constraint.locator.appending(.returnType)))
      return true

    case (let lhs as FunParamType, let rhs as FunParamType):
      var simplified = RelationalConstraint(
        kind: constraint.kind, lhs: lhs.rawType, rhs: rhs.rawType, at: constraint.locator)

      // If RHS is consuming, LHS can have any passing policy. Otherwise, policies should match.
      if (lhs.policy != rhs.policy) && (rhs.policy != .consuming) {
        errors.append(.conflictingPolicies(constraint))
      }

      // If RHS is mutating, then LHS must have the raw type as RHS.
      if rhs.policy == .inout {
        simplified.kind = .equality
      }

      system.insert(simplified)
      return true

    case (let lhs as AsyncType, let rhs as AsyncType):
      system.insert(RelationalConstraint(
        kind: constraint.kind, lhs: lhs.base, rhs: rhs.base,
        at: constraint.locator))
      return true

    case (let lhs as KindType, let rhs as KindType):
      system.insert(RelationalConstraint(
        kind: constraint.kind, lhs: lhs.type, rhs: rhs.type,
        at: constraint.locator))
      return true

    default:
      return false
    }
  }

}

fileprivate struct SolutionSet<T> {

  typealias Element = (id: T, solution: Solution)

  var results: [Element] = []

  mutating func insert(_ newElement: Element) -> Solution.Score {
    if results.isEmpty || (newElement.solution.score < results[0].solution.score) {
      results = [newElement]
    } else if newElement.solution.score == results[0].solution.score {
      // Don't insert duplicates.
      if !results.contains(where: { $0.solution ~= newElement.solution }) {
        results.append(newElement)
      }
    }
    return results[0].solution.score
  }

  var isEmpty: Bool { results.isEmpty }

  var best: Element? {
    if results.isEmpty {
      return nil
    } else if results.count == 1 {
      return results[0]
    }

    // FIXME: Sort solutions by the number of coercions they have to do.
    // We reached this point because there's was no way to identify a unique best solution.
    return nil
  }

  func map<T>(_ transform: (Element) -> T) -> [T] {
    return results.map(transform)
  }

}
