import Utils

/// A constraint system solver.
struct ConstraintSolver {

  /// The scope in which the constraints are solved.
  private let scope: AnyScopeID

  /// The fresh constraints to solve.
  private var fresh: [LocatableConstraint] = []

  /// The constraints that are currently stale.ß
  private var stale: [LocatableConstraint] = []

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
    fresh: [LocatableConstraint],
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

      switch constraint.constraint {
      case .conformance(let l, let traits):
        solve(l, conformsTo: traits, location: constraint.location, using: &checker)
      case .equality(let l, let r):
        solve(l, equalsTo: r, location: constraint.location, using: &checker)
      case .subtyping(let l, let r):
        solve(l, isSubtypeOf: r, location: constraint.location, using: &checker)
      case .parameter(let l, let r):
        solve(l, passableTo: r, location: constraint.location, using: &checker)
      case .boundMember(let l, let m, let r):
        solve(l, hasBoundMember: m, ofType: r, location: constraint.location, using: &checker)
      case .unboundMember(let l, let m, let r):
        solve(l, hasUnboundMember: m, ofType: r, location: constraint.location, using: &checker)
      case .disjunction:
        return solve(disjunction: constraint, using: &checker)
      case .overload:
        return solve(overload: constraint, using: &checker)
      default:
        fatalError("not implemented")
      }
    }

    return finalize(using: &checker)
  }

  /// Eliminates `L : T1 & ... & Tn` if the solver has enough information to check whether or not
  /// `L` conforms to each trait `Ti`. Otherwise, postpones the constraint.
  private mutating func solve(
    _ l: Type,
    conformsTo traits: Set<TraitType>,
    location: LocatableConstraint.Location,
    using checker: inout TypeChecker
  ) {
    let l = typeAssumptions[l]

    switch l {
    case .variable:
      // Postpone the solving if `L` is still unknown.
      postpone(LocatableConstraint(.conformance(l: l, traits: traits), location: location))

    case .product, .tuple:
      let conformedTraits = checker.conformedTraits(of: l, inScope: scope) ?? []
      let nonConforming = traits.subtracting(conformedTraits)

      if !nonConforming.isEmpty {
        for trait in nonConforming {
          diagnostics.append(.noConformance(of: l, to: trait, at: location.origin))
        }
      }

    default:
      fatalError("not implemented")
    }
  }

  /// Eliminates `L == R` by unifying `L` with `R`.
  private mutating func solve(
    _ l: Type,
    equalsTo r: Type,
    location: LocatableConstraint.Location,
    using checker: inout TypeChecker
  ) {
    let l = typeAssumptions[l]
    let r = typeAssumptions[r]

    if l == r { return }

    switch (l, r) {
    case (.variable(let tau), _):
      typeAssumptions.assign(r, to: tau)
      refresh(constraintsDependingOn: tau)

    case (_, .variable(let tau)):
      typeAssumptions.assign(l, to: tau)
      refresh(constraintsDependingOn: tau)

    case (.tuple(let l), .tuple(let r)):
      switch l.testLabelCompatibility(with: r) {
      case .differentLengths:
        diagnostics.append(.incompatibleTupleLengths(at: location.origin))
        return

      case .differentLabels(let found, let expected):
        diagnostics.append(.incompatibleLabels(
          found: found, expected: expected, at: location.origin))
        return

      case .compatible:
        break
      }

      // Break down the constraint.
      for i in 0 ..< l.elements.count {
        fresh.append(LocatableConstraint(
          .equality(l: l.elements[i].type, r: r.elements[i].type), location: location))
      }

    case (.lambda(let l), .lambda(let r)):
      switch l.testLabelCompatibility(with: r) {
      case .differentLengths:
        diagnostics.append(.incompatibleParameterCount(at: location.origin))
        return

      case .differentLabels(let found, let expected):
        diagnostics.append(.incompatibleLabels(
          found: found, expected: expected, at: location.origin))
        return

      case .compatible:
        break
      }

      // Break down the constraint.
      for i in 0 ..< l.inputs.count {
        fresh.append(LocatableConstraint(
          .equality(l: l.inputs[i].type, r: r.inputs[i].type), location: location))
      }
      fresh.append(LocatableConstraint(
        .equality(l: l.output, r: r.output), location: location))
      fresh.append(LocatableConstraint(
        .equality(l: l.environment, r: r.environment), location: location))

    case (.method(let l), .method(let r)):
      // Capabilities must match.
      if l.capabilities != r.capabilities {
        diagnostics.append(.incompatibleTypes(.method(l), .method(r), at: location.origin))
        return
      }

      // Break down the constraint.
      for i in 0 ..< l.inputs.count {
        fresh.append(LocatableConstraint(
          .equality(l: l.inputs[i].type, r: r.inputs[i].type), location: location))
      }
      fresh.append(LocatableConstraint(
        .equality(l: l.output, r: r.output), location: location))
      fresh.append(LocatableConstraint(
        .equality(l: l.receiver, r: r.receiver), location: location))

    case (.method(let l), .lambda):
      // TODO: Use a kind of different constraint for call exprs
      // We can't guess the operator property and environment of a callee from a call expression;
      // that must be inferred. Thus we can't constrain the callee of a CallExpr to be equal to
      // some synthesized lambda type. Instead we need a constraint that only describes its inputs
      // and outputs, and uses the other type to infer additional information.

      var minterms: [Constraint.Minterm] = []

      if let lambda = LambdaType(letImplOf: l) {
        minterms.append(Constraint.Minterm(
          constraints: [.equality(l: .lambda(lambda), r: r)], penalties: 0))
      }
      if let lambda = LambdaType(inoutImplOf: l) {
        minterms.append(Constraint.Minterm(
          constraints: [.equality(l: .lambda(lambda), r: r)], penalties: 1))
      }
      if let lambda = LambdaType(sinkImplOf: l) {
        minterms.append(Constraint.Minterm(
          constraints: [.equality(l: .lambda(lambda), r: r)], penalties: 1))
      }

      if minterms.count == 1 {
        fresh.append(LocatableConstraint(minterms[0].constraints[0], location: location))
      } else {
        assert(!minterms.isEmpty)
        fresh.append(LocatableConstraint(Constraint.disjunction(minterms), location: location))
      }

    default:
      diagnostics.append(.incompatibleTypes(l, r, at: location.origin))
    }
  }

  /// Eliminates `L <: R` if the solver has enough information to check that `L` is subtype of `R`
  /// or must be unified with `R`. Otherwise, postpones the constraint.
  private mutating func solve(
    _ l: Type,
    isSubtypeOf r: Type,
    location: LocatableConstraint.Location,
    using checker: inout TypeChecker
  ) {
    let l = typeAssumptions[l]
    let r = typeAssumptions[r]

    if l == r { return }

    switch (l, r) {
    case (_, .variable):
      // The type variable is above a more concrete type. We should compute the "join" of all types
      // to which `L` is coercible and that are below `R`, but that set is unbounded. We have no
      // choice to postpone the constraint.
      postpone(LocatableConstraint(.subtyping(l: l, r: r), location: location))

    case (.variable, _):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // coercible to `R` and that are above `L`, but that set is unbounded unless `R` is a leaf.
      // If it isn't, we have no choice but to postpone the constraint.
      if r.isLeaf {
        solve(l, equalsTo: r, location: location, using: &checker)
      } else {
        postpone(LocatableConstraint(.subtyping(l: l, r: r), location: location))
      }

    case (_, .existential):
      // All types conform to any.
      if r == .any { return }
      fatalError("not implemented")

    case (_, .lambda):
      fatalError("not implemented")

    case (_, .union):
      fatalError("not implemented")

    default:
      diagnostics.append(.notSubtype(l, of: r, at: location.origin))
    }
  }

  /// Eliminates `L ⤷ R` if the solver has enough information to choose whether the constraint can
  /// be simplified as equality or subtyping. Otherwise, postpones the constraint.
  private mutating func solve(
    _ l: Type,
    passableTo r: Type,
    location: LocatableConstraint.Location,
    using checker: inout TypeChecker
  ) {
    let l = typeAssumptions[l]
    let r = typeAssumptions[r]

    if l == r { return }

    switch r {
    case .variable:
      // Postpone the solving until we can infer the parameter passing convention of `R`.
      postpone(LocatableConstraint(.parameter(l: l, r: r), location: location))

    case .parameter(let p):
      // Either `L` is equal to the bare type of `R`, or it's a. Note: the equality requirement for
      // arguments passed mutably is verified after type inference.
      fresh.append(LocatableConstraint(
        .equalityOrSubtyping(l: l, r: p.bareType), location: location))

    default:
      diagnostics.append(.invalidParameterType(r, at: location.origin))
    }
  }

  /// Simplifies `bound(L.m) == R` as an overload or equality constraint unifying `R` with the
  /// bound type of `L.m` if the solver has enough information to resolve `m` as a bound member.
  /// Otherwise, postones the constraint.
  private mutating func solve(
    _ l: Type,
    hasBoundMember member: Name,
    ofType r: Type,
    location: LocatableConstraint.Location,
    using checker: inout TypeChecker
  ) {
    // Postpone the solving if `L` is still unknown.
    let l = typeAssumptions[l]
    if case .variable = l {
      postpone(LocatableConstraint(.boundMember(l: l, m: member, r: r), location: location))
      return
    }

    // Search for non-static members with the specified name.
    let allMatches = checker.lookup(member.stem, memberOf: l, inScope: scope)
    let nonStaticMatches = allMatches.filter({ decl in
      checker.program.isNonStaticMember(decl)
    })

    // Catch uses of static members on instances.
    if nonStaticMatches.isEmpty && !allMatches.isEmpty {
      diagnostics.append(.staticMemberUsedOnInstance(member: member, type: l, at: location.origin))
    }

    // Generate the list of candidates.
    let candidates = nonStaticMatches.compactMap({ (match) -> Constraint.OverloadCandidate? in
      // Realize the type of the declaration and skip it if that fails.
      let matchType = checker.realize(decl: match)
      if matchType.isError { return nil }

      // TODO: Handle bound generic typess

      return Constraint.OverloadCandidate(
        reference: .member(match),
        type: matchType,
        constraints: [],
        penalties: checker.program.isRequirement(match) ? 1 : 0)
    })

    // Fail if we couldn't find any candidate.
    if candidates.isEmpty {
      diagnostics.append(.undefined(name: "\(member)", at: location.origin))
      return
    }

    // Get the name expression associated with the constraint, if any.
    let nameBoundByCurrentConstraint = location.node.flatMap(NodeID<NameExpr>.init)

    // Fast path when there's only one candidate.
    if candidates.count == 1 {
      solve(candidates[0].type, equalsTo: r, location: location, using: &checker)
      if let name = nameBoundByCurrentConstraint {
        bindingAssumptions[name] = candidates[0].reference
      }
    }

    let newConstraint: Constraint
    if let name = nameBoundByCurrentConstraint {
      newConstraint = .overload(name: name, type: r, candidates: candidates)
    } else {
      newConstraint = .disjunction(candidates.map({ (c) -> Constraint.Minterm in
        Constraint.Minterm(constraints: [.equality(l: r, r: c.type)], penalties: c.penalties)
      }))
    }
    fresh.append(LocatableConstraint(newConstraint, location: location))
  }

  /// Simplifies `bound(L.m) == R` as an overload or equality constraint unifying `R` with the
  /// unbound type of `L.m` if the solver has enough information to resolve `m` as a bound member.
  /// Otherwise, postones the constraint.
  private mutating func solve(
    _ l: Type,
    hasUnboundMember member: Name,
    ofType r: Type,
    location: LocatableConstraint.Location,
    using checker: inout TypeChecker
  ) {
    // Postpone the solving if `L` is still unknown.
    let l = typeAssumptions[l]
    if case .variable = l {
      postpone(LocatableConstraint(.unboundMember(l: l, m: member, r: r), location: location))
      return
    }

    // Search for non-static members with the specified name.
    let matches = checker.lookup(member.stem, memberOf: l, inScope: scope)

    // Generate the list of candidates.
    typealias Candidate = (decl: AnyDeclID, type: Type, penalty: Int)
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
      diagnostics.append(.undefined(name: "\(member)", at: location.origin))
      return
    }

    // Fast path when there's only one candidate.
    if candidates.count == 1 {
      solve(candidates[0].type, equalsTo: r, location: location, using: &checker)
      if let node = location.node,
         let name = NodeID<NameExpr>(node)
      {
        bindingAssumptions[name] = .direct(candidates[0].decl)
      }
    }

    // TODO: Create an overload constraint
    fatalError("not implemented")
  }

  /// Attempts to solve the remaining constraints for each individual choice in `disjunction` and
  /// returns the best solution.
  private mutating func solve(
    disjunction: LocatableConstraint,
    using checker: inout TypeChecker
  ) -> Solution? {
    guard case .disjunction(let minterms) = disjunction.constraint else { unreachable() }

    return explore(
      minterms,
      location: disjunction.location,
      using: &checker,
      insertingConstraintsWith: { (subsolver, choice) -> Void in
        for constraint in choice.constraints {
          subsolver.fresh.append(LocatableConstraint(constraint, location: disjunction.location))
        }
      })?.solution
  }

  /// Attempts to solve the remaining constraints with each individual choice in `overload` and
  /// returns the best solution.
  private mutating func solve(
    overload: LocatableConstraint,
    using checker: inout TypeChecker
  ) -> Solution? {
    guard case .overload(let name, let type, let candidates) = overload.constraint else {
      unreachable()
    }

    let bestChoice = explore(
      candidates,
      location: overload.location,
      using: &checker,
      insertingConstraintsWith: { (subsolver, choice) -> Void in
        subsolver.fresh.append(LocatableConstraint(
          .equality(l: type, r: choice.type), location: overload.location))
        for constraint in choice.constraints {
          subsolver.fresh.append(LocatableConstraint(constraint, location: overload.location))
        }
      })

    if let (choice, solution) = bestChoice {
      bindingAssumptions[name] = choice.reference
      return solution
    } else {
      return nil
    }
  }

  /// Solves the remaining constraint with each given choice and returns the best solution along
  /// with the choice that produced it.
  private mutating func explore<C: Collection>(
    _ choices: C,
    location: LocatableConstraint.Location,
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
      results[0].solution.diagnose(.ambiguousDisjunction(at: location.origin))
      return results[0]
    }
  }

  /// Schedules `constraint` to be solved later.
  private mutating func postpone(_ constraint: LocatableConstraint) {
    stale.append(constraint)
  }

  /// Moves the stale constraints depending on the specified variables back to the fresh set.
  private mutating func refresh(constraintsDependingOn variable: TypeVariable) {
    for i in (0 ..< stale.count).reversed() {
      if stale[i].constraint.depends(on: variable) {
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
      s.diagnose(.staleConstraint(constraint: c.constraint, at: c.location.origin))
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

extension Constraint.Minterm: Choice {}

extension Constraint.OverloadCandidate: Choice {}
