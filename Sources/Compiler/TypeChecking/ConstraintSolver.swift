import Utils

/// A constraint system solver.
struct ConstraintSolver {

  /// The scope in which the constraints are solved.
  private let scope: AnyScopeID

  /// The fresh constraints to solve.
  private var fresh: [Constraint] = []

  /// The constraints that are currently stale.ß
  private var stale: [Constraint] = []

  /// The type assumptions of the solver.
  private var typeAssumptions = SubstitutionMap()

  /// The binding assumptions of the solver.
  private var bindingAssumptions: [NodeID<NameExpr>: DeclRef] = [:]

  /// The current penalties of the solver's solution.
  private var penalties: Int = 0

  /// The diagnostics of the errors the solver encountered.
  private var diagnostics: [Diagnostic]

  /// The score of the best solution computed so far.
  private var best = Solution.Score.worst

  /// Creates an instance that solves the constraints in `fresh` in `scope`, returning a solution
  /// populated with `initialDiagnostics`.
  init(
    scope: AnyScopeID,
    fresh: [Constraint],
    initialDiagnostics: [Diagnostic] = []
  ) {
    self.scope = scope
    self.fresh = fresh
    self.diagnostics = initialDiagnostics
  }

  /// The current score of the solver's solution.
  private var score: Solution.Score {
    Solution.Score(errorCount: diagnostics.count, penalties: penalties)
  }

  /// Applies `self` to solve its constraints using `checker` to resolve names and realize types.
  mutating func apply(using checker: inout TypeChecker) -> Solution {
    solve(using: &checker)!
  }

  /// Solves the constraints and returns the best solution, or `nil` if a better solution has
  /// already been computed.
  private mutating func solve(using checker: inout TypeChecker) -> Solution? {
    while let constraint = fresh.popLast() {
      // Make sure the current solution is still worth exploring.
      if score > best { return nil }

      switch constraint {
      case let c as ConformanceConstraint:
        solve(conformance: c, using: &checker)
      case let c as EqualityConstraint:
        solve(equality: c)
      case let c as SubtypingConstraint:
        solve(subtyping: c)
      case let c as ParameterConstraint:
        solve(parameter: c)
      case let c as BoundMemberConstraint:
        solve(boundMember: c, using: &checker)
      case let c as UnboundMemberConstraint:
        solve(unboundMember: c, using: &checker)
      case let c as DisjunctionConstraint:
        return solve(disjunction: c, using: &checker)
      case let c as OverloadConstraint:
        return solve(overload: c, using: &checker)
      default:
        unreachable()
      }
    }

    return finalize(using: &checker)
  }

  /// Eliminates `L : T1 & ... & Tn` if the solver has enough information to check whether or not
  /// `L` conforms to each trait `Ti`. Otherwise, postpones the constraint.
  private mutating func solve(
    conformance constraint: ConformanceConstraint,
    using checker: inout TypeChecker
  ) {
    let subject = typeAssumptions[constraint.subject]

    switch subject.base {
    case is TypeVariable:
      // Postpone the solving if `L` is still unknown.
      postpone(
        ConformanceConstraint(subject, traits: constraint.traits, because: constraint.cause))

    case is ProductType, is TupleType:
      let conformedTraits = checker.conformedTraits(of: subject, inScope: scope) ?? []
      let nonConforming = constraint.traits.subtracting(conformedTraits)

      if !nonConforming.isEmpty {
        for trait in nonConforming {
          diagnostics.append(
            .diagnose(constraint.subject, doesNotConformTo: trait, at: constraint.cause.origin))
        }
      }

    default:
      fatalError("not implemented")
    }
  }

  /// Eliminates `L == R` by unifying `L` with `R`.
  private mutating func solve(equality constraint: EqualityConstraint) {
    let l = typeAssumptions[constraint.left]
    let r = typeAssumptions[constraint.right]

    if l == r { return }

    switch (l.base, r.base) {
    case (let tau as TypeVariable, _):
      typeAssumptions.assign(r, to: tau)
      refresh(constraintsDependingOn: tau)

    case (_, let tau as TypeVariable):
      typeAssumptions.assign(l, to: tau)
      refresh(constraintsDependingOn: tau)

    case (let l as TupleType, let r as TupleType):
      switch l.testLabelCompatibility(with: r) {
      case .differentLengths:
        diagnostics.append(.diagnose(incompatibleTupleLengthsAt: constraint.cause.origin))
        return

      case .differentLabels(let found, let expected):
        diagnostics.append(.diagnose(
          labels: found, incompatibleWith: expected, at: constraint.cause.origin))
        return

      case .compatible:
        break
      }

      // Break down the constraint.
      for i in 0 ..< l.elements.count {
        solve(equality: .init(l.elements[i].type, r.elements[i].type, because: constraint.cause))
      }

    case (let l as LambdaType, let r as LambdaType):
      switch l.testLabelCompatibility(with: r) {
      case .differentLengths:
        diagnostics.append(.diagnose(incompatibleParameterCountAt: constraint.cause.origin))
        return

      case .differentLabels(let found, let expected):
        diagnostics.append(.diagnose(
          labels: found, incompatibleWith: expected, at: constraint.cause.origin))
        return

      case .compatible:
        break
      }

      // Break down the constraint.
      for i in 0 ..< l.inputs.count {
        solve(equality: .init(l.inputs[i].type, r.inputs[i].type, because: constraint.cause))
      }

      solve(equality: .init(l.output, r.output, because: constraint.cause))
      solve(equality: .init(l.environment, r.environment, because: constraint.cause))

    case (let l as MethodType, let r as MethodType):
      // Capabilities must match.
      if l.capabilities != r.capabilities {
        diagnostics.append(.diagnose(type: ^l, incompatibleWith: ^r, at: constraint.cause.origin))
        return
      }

      // Break down the constraint.
      for i in 0 ..< l.inputs.count {
        solve(equality: .init(l.inputs[i].type, r.inputs[i].type, because: constraint.cause))
      }

      solve(equality: .init(l.output, r.output, because: constraint.cause))
      solve(equality: .init(l.receiver, r.receiver, because: constraint.cause))

    case (let l as MethodType, _ as LambdaType):
      // TODO: Use a different kind of constraint for call exprs
      // We can't guess the operator property and environment of a callee from a call expression;
      // that must be inferred. Thus we can't constrain the callee of a CallExpr to be equal to
      // some synthesized lambda type. Instead we need a constraint that only describes its inputs
      // and outputs, and uses the other type to infer additional information.

      var minterms: [DisjunctionConstraint.Choice] = []

      if let lambda = LambdaType(letImplOf: l) {
        minterms.append(
          .init(
            constraints: [EqualityConstraint(^lambda, r, because: constraint.cause)],
            penalties: 0))
      }

      if let lambda = LambdaType(inoutImplOf: l) {
        minterms.append(
          .init(
            constraints: [EqualityConstraint(^lambda, r, because: constraint.cause)],
            penalties: 1))
      }

      if let lambda = LambdaType(sinkImplOf: l) {
        minterms.append(
          .init(
            constraints: [EqualityConstraint(^lambda, r, because: constraint.cause)],
            penalties: 1))
      }

      if minterms.count == 1 {
        solve(equality: minterms[0].constraints.first as! EqualityConstraint)
      } else {
        schedule(DisjunctionConstraint(choices: minterms, because: constraint.cause))
      }

    default:
      diagnostics.append(.diagnose(type: l, incompatibleWith: r, at: constraint.cause.origin))
    }
  }

  /// Eliminates `L <: R` if the solver has enough information to check that `L` is subtype of `R`
  /// or must be unified with `R`. Otherwise, postpones the constraint.
  private mutating func solve(subtyping constraint: SubtypingConstraint) {
    let l = typeAssumptions[constraint.left]
    let r = typeAssumptions[constraint.right]

    if l == r { return }

    switch (l.base, r.base) {
    case (_, _ as TypeVariable):
      // The type variable is above a more concrete type. We should compute the "join" of all types
      // to which `L` is coercible and that are below `R`, but that set is unbounded. We have no
      // choice to postpone the constraint.
      postpone(SubtypingConstraint(l, r, because: constraint.cause))

    case (_ as TypeVariable, _):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // coercible to `R` and that are above `L`, but that set is unbounded unless `R` is a leaf.
      // If it isn't, we have no choice but to postpone the constraint.
      if r.isLeaf {
        solve(equality: .init(constraint))
      } else {
        postpone(SubtypingConstraint(l, r, because: constraint.cause))
      }

    case (_, _ as ExistentialType):
      // All types conform to any.
      if r == .any { return }
      fatalError("not implemented")

    case (_, _ as LambdaType):
      fatalError("not implemented")

    case (_, _ as UnionType):
      fatalError("not implemented")

    default:
      diagnostics.append(.diagnose(type: l, isNotSubtypeOf: r, at: constraint.cause.origin))
    }
  }

  /// Eliminates `L ⤷ R` if the solver has enough information to choose whether the constraint can
  /// be simplified as equality or subtyping. Otherwise, postpones the constraint.
  private mutating func solve(parameter constraint: ParameterConstraint) {
    let l = typeAssumptions[constraint.left]
    let r = typeAssumptions[constraint.right]

    if l == r { return }

    switch r.base {
    case is TypeVariable:
      // Postpone the solving until we can infer the parameter passing convention of `R`.
      postpone(ParameterConstraint(l, r, because: constraint.cause))

    case let p as ParameterType:
      // Either `L` is equal to the bare type of `R`, or it's a. Note: the equality requirement for
      // arguments passed mutably is verified after type inference.
      schedule(
        equalityOrSubtypingConstraint(l, p.bareType, because: constraint.cause))

    default:
      diagnostics.append(.diagnose(invalidParameterType: r, at: constraint.cause.origin))
    }
  }

  /// Simplifies `bound(L.m) == R` as an overload or equality constraint unifying `R` with the
  /// bound type of `L.m` if the solver has enough information to resolve `m` as a bound member.
  /// Otherwise, postones the constraint.
  private mutating func solve(
    boundMember constraint: BoundMemberConstraint,
    using checker: inout TypeChecker
  ) {
    let l = typeAssumptions[constraint.left]
    let r = typeAssumptions[constraint.right]

    // Postpone the solving if `L` is still unknown.
    if l.base is TypeVariable {
      postpone(BoundMemberConstraint(
        type: l,
        hasMemberNamed: constraint.member,
        ofType: r,
        because: constraint.cause))
      return
    }

    // Search for non-static members with the specified name.
    let allMatches = checker.lookup(constraint.member.stem, memberOf: l, inScope: scope)
    let nonStaticMatches = allMatches.filter({ decl in
      checker.program.isNonStaticMember(decl)
    })

    // Catch uses of static members on instances.
    if nonStaticMatches.isEmpty && !allMatches.isEmpty {
      diagnostics.append(.diagnose(
        illegalUseOfStaticMember: constraint.member,
        onInstanceOf: l,
        at: constraint.cause.origin))
    }

    // Generate the list of candidates.
    let candidates = nonStaticMatches.compactMap({ (match) -> OverloadConstraint.Candidate? in
      // Realize the type of the declaration and skip it if that fails.
      let matchType = checker.realize(decl: match)
      if matchType.isError { return nil }

      // TODO: Handle bound generic typess

      return OverloadConstraint.Candidate(
        reference: .member(match),
        type: matchType,
        constraints: [],
        penalties: checker.program.isRequirement(match) ? 1 : 0)
    })

    // Fail if we couldn't find any candidate.
    if candidates.isEmpty {
      diagnostics.append(.diagnose(
        undefinedName: "\(constraint.member)",
        at: constraint.cause.origin))
      return
    }

    // If there's only one candidate, solve an equality constraint direcly.
    if candidates.count == 1 {
      solve(equality: .init(candidates[0].type, r, because: constraint.cause))
      if let name = constraint.memberExpr {
        bindingAssumptions[name] = candidates[0].reference
      }
      return
    }

    // If there are several candidates, create a disjunction constraint.
    if let name = constraint.memberExpr {
      schedule(OverloadConstraint(
        name,
        withType: r,
        refersToOneOf: candidates,
        because: constraint.cause))
    } else {
      schedule(DisjunctionConstraint(
        choices: candidates.map({ (c) -> DisjunctionConstraint.Choice in
          .init(
            constraints: [EqualityConstraint(r, c.type, because: constraint.cause)],
            penalties: c.penalties)
        }),
        because: constraint.cause))
    }
  }

  /// Simplifies `unbound(L.m) == R` as an overload or equality constraint unifying `R` with the
  /// unbound type of `L.m` if the solver has enough information to resolve `m` as a bound member.
  /// Otherwise, postones the constraint.
  private mutating func solve(
    unboundMember constraint: UnboundMemberConstraint,
    using checker: inout TypeChecker
  ) {
    let l = typeAssumptions[constraint.left]
    let r = typeAssumptions[constraint.right]

    // Postpone the solving if `L` is still unknown.
    if l.base is TypeVariable {
      postpone(UnboundMemberConstraint(
        type: l,
        hasMemberNamed: constraint.member,
        ofType: r,
        because: constraint.cause))
      return
    }

    // Search for non-static members with the specified name.
    let matches = checker.lookup(constraint.member.stem, memberOf: l, inScope: scope)

    // Generate the list of candidates.
    typealias Candidate = (decl: AnyDeclID, type: AnyType, penalty: Int)
    let candidates = matches.compactMap({ (match) -> Candidate? in
      // Realize the type of the declaration and skip it if that fails.
      let matchType = checker.realize(decl: match)
      if matchType.isError { return nil }

      // TODO: Handle bound generic typess
      // TODO: Handle static access to bound members
      assert(checker.program.isGlobal(match), "not implemented")

      return (
        decl: match,
        type: matchType,
        penalty: checker.program.isRequirement(match) ? 1 : 0)
    })

    // Fail if we couldn't find any candidate.
    if candidates.isEmpty {
      diagnostics.append(.diagnose(
        undefinedName: "\(constraint.member)",
        at: constraint.cause.origin))
      return
    }

    // If there's only one candidate, solve an equality constraint direcly.
    if candidates.count == 1 {
      solve(equality: .init(candidates[0].type, r, because: constraint.cause))
      if let name = constraint.memberExpr {
        bindingAssumptions[name] = .direct(candidates[0].decl)
      }
    }

    // TODO: Create an overload constraint
    fatalError("not implemented")
  }

  /// Attempts to solve the remaining constraints for each individual choice in `disjunction` and
  /// returns the best solution.
  private mutating func solve(
    disjunction constraint: DisjunctionConstraint,
    using checker: inout TypeChecker
  ) -> Solution? {
    let bestChoice = explore(
      constraint.choices,
      cause: constraint.cause,
      using: &checker,
      insertingConstraintsWith: { (subsolver, choice) -> Void in
        for c in choice.constraints {
          var subConstraint = c
          subConstraint.cause = constraint.cause
          subsolver.schedule(subConstraint)
        }
      })

    return bestChoice?.solution
  }

  /// Attempts to solve the remaining constraints with each individual choice in `overload` and
  /// returns the best solution.
  private mutating func solve(
    overload constraint: OverloadConstraint,
    using checker: inout TypeChecker
  ) -> Solution? {
    let bestChoice = explore(
      constraint.choices,
      cause: constraint.cause,
      using: &checker,
      insertingConstraintsWith: { (subsolver, choice) -> Void in
        subsolver.schedule(EqualityConstraint(
          constraint.overloadedExprType,
          choice.type,
          because: constraint.cause))

        for c in choice.constraints {
          var subConstraint = c
          subConstraint.cause = constraint.cause
          subsolver.schedule(subConstraint)
        }
      })

    if let (choice, solution) = bestChoice {
      bindingAssumptions[constraint.overloadedExpr] = choice.reference
      return solution
    } else {
      return nil
    }
  }

  /// Solves the remaining constraint with each given choice and returns the best solution along
  /// with the choice that produced it.
  private mutating func explore<C: Collection>(
    _ choices: C,
    cause: ConstraintCause?,
    using checker: inout TypeChecker,
    insertingConstraintsWith insertConstraints: (inout ConstraintSolver, C.Element) -> Void
  ) -> (choice: C.Element, solution: Solution)?
  where C.Element: Choice
  {
    /// The results of the exploration.
    var results: [(choice: C.Element, solution: Solution)] = []

    for choice in choices {
      // Don't bother if there's no chance to find a better solution.
      var underestimatedChoiceScore = score
      underestimatedChoiceScore.penalties += choice.penalties
      if underestimatedChoiceScore > best {
        continue
      }

      // Explore the result of this choice.
      var subsolver = self
      subsolver.penalties += choice.penalties
      insertConstraints(&subsolver, choice)

      guard let solution = subsolver.solve(using: &checker) else { continue }
      if results.isEmpty || (solution.score < best) {
        best = solution.score
        results = [(choice, solution)]
      } else if solution.score == best {
        // TODO: Avoid duplicates
        results.append((choice, solution))
      }
    }

    switch results.count {
    case 0:
      return nil

    case 1:
      return results[0]

    default:
      // TODO: Merge remaining solutions
      results[0].solution.addDiagnostic(.diagnose(ambiguousDisjunctionAt: cause?.origin))
      return results[0]
    }
  }

  /// Schedules `constraint` to be solved.
  private mutating func schedule(_ constraint: Constraint) {
    fresh.append(constraint)
  }

  /// Schedules `constraint` to be solved once the solver has inferred more information about its
  /// type variables.
  private mutating func postpone(_ constraint: Constraint) {
    stale.append(constraint)
  }

  /// Moves the stale constraints depending on the specified variables back to the fresh set.
  private mutating func refresh(constraintsDependingOn variable: TypeVariable) {
    for i in (0 ..< stale.count).reversed() {
      if stale[i].depends(on: variable) {
        fresh.append(stale.remove(at: i))
      }
    }
  }

  /// Creates a solution from the current state.
  private func finalize(using checker: inout TypeChecker) -> Solution {
    assert(fresh.isEmpty)
    var s = Solution(
      typeAssumptions: typeAssumptions.flattened(),
      bindingAssumptions: bindingAssumptions,
      penalties: penalties,
      diagnostics: diagnostics)

    for c in stale {
      s.addDiagnostic(.diagnose(staleConstraint: c))
    }

    return s
  }

}

/// The result of a label compatibility test.
fileprivate enum LabelCompatibility {

  case compatible

  case differentLengths

  case differentLabels(found: [String?], expected: [String?])

}

/// A collection of labeled elements.
fileprivate protocol LabeledCollection {

  associatedtype Labels: Collection where Labels.Element == String?

  /// A collection with the labels corresponding to each element in `self`.
  var labels: Labels { get }

}

extension LabeledCollection {

  /// Tests whether `self` and `other` have compatible labels.
  func testLabelCompatibility<T: LabeledCollection>(with other: T) -> LabelCompatibility {
    let (ls, rs) = (self.labels, other.labels)

    if ls.count != rs.count {
      return .differentLengths
    }
    for (l, r) in zip(ls, rs) {
      if l != r { return .differentLabels(found: Array(ls), expected: Array(rs)) }
    }
    return .compatible
  }

}

extension LambdaType: LabeledCollection {

  var labels: LazyMapSequence<LazySequence<Array<CallableTypeParameter>>.Elements, String?> {
    inputs.lazy.map({ $0.label })
  }

}

extension TupleType: LabeledCollection {

  var labels: LazyMapSequence<LazySequence<Array<TupleType.Element>>.Elements, String?> {
    elements.lazy.map({ $0.label })
  }

}

/// A type representing a choice during constraint solving.
fileprivate protocol Choice {

  /// The penalties associated with the choice.
  var penalties: Int { get }

}

extension DisjunctionConstraint.Choice: Choice {}

extension OverloadConstraint.Candidate: Choice {}
