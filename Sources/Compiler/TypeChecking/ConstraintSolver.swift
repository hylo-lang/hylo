import Utils

/// A constraint system solver.
struct ConstraintSolver {

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker!

  /// The scope in which the constraints are solved.
  let scope: AnyScopeID

  /// The fresh constraints to solve.
  var fresh: [LocatableConstraint] = []

  /// The constraints that are currently stale.ß
  var stale: [LocatableConstraint] = []

  /// The type assumptions of the solver.
  var typeAssumptions = SubstitutionMap()

  /// The binding assumptions of the solver.
  var bindingAssumptions: [NodeID<NameExpr>: DeclRef] = [:]

  /// The current penalties of the solver's solution.
  var penalties: Int = 0

  /// The diagnostics of the errors the solver encountered.
  var diagnostics: [Diagnostic] = []

  /// The score of the best solution computed so far.
  var best = Solution.Score.worst

  /// The current score of the solver's solution.
  var score: Solution.Score {
    Solution.Score(errorCount: diagnostics.count, penalties: penalties)
  }

  /// Solves the constraints and returns the best solution, or `nil` if a better solution has
  /// already been computed.
  mutating func solve() -> Solution? {
    while let constraint = fresh.popLast() {
      // Make sure the current solution is still worth exploring.
      if score > best { return nil }

      switch constraint.constraint {
      case .conformance(let l, let traits):
        solve(l, conformsTo: traits, location: constraint.location)
      case .equality(let l, let r):
        solve(l, equalsTo: r, location: constraint.location)
      case .subtyping(let l, let r):
        solve(l, isSubtypeOf: r, location: constraint.location)
      case .parameter(let l, let r):
        solve(l, passableTo: r, location: constraint.location)
      case .member(let l, let m, let r):
        solve(l, hasMember: m, ofType: r, location: constraint.location)
      case .disjunction:
        return solve(disjunction: constraint)
      default:
        fatalError("not implemented")
      }
    }

    return finalize()
  }

  private mutating func solve(
    _ l: Type,
    conformsTo traits: Set<TraitType>,
    location: LocatableConstraint.Location
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
          diagnostics.append(.noConformance(of: l, to: trait, range: range(of: location)))
        }
      }

    default:
      fatalError("not implemented")
    }
  }

  private mutating func solve(
    _ l: Type,
    equalsTo r: Type,
    location: LocatableConstraint.Location
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
        diagnostics.append(.incompatibleTupleLengths(range: range(of: location)))
        return

      case .differentLabels(let found, let expected):
        diagnostics.append(.incompatibleLabels(
          found: found, expected: expected, range: range(of: location)))
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
        diagnostics.append(.incompatibleParameterCount(range: range(of: location)))
        return

      case .differentLabels(let found, let expected):
        diagnostics.append(.incompatibleLabels(
          found: found, expected: expected, range: range(of: location)))
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
        diagnostics.append(.incompatibleTypes(.method(l), .method(r), range: range(of: location)))
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
      diagnostics.append(.incompatibleTypes(l, r, range: range(of: location)))
    }
  }

  private mutating func solve(
    _ l: Type,
    isSubtypeOf r: Type,
    location: LocatableConstraint.Location
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
        solve(l, equalsTo: r, location: location)
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
      diagnostics.append(.notSubtype(l, of: r, range: range(of: location)))
    }
  }

  private mutating func solve(
    _ l: Type,
    passableTo r: Type,
    location: LocatableConstraint.Location
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
      diagnostics.append(.invalidParameterType(r, range: range(of: location)))
    }
  }

  private mutating func solve(
    _ l: Type,
    hasMember member: Name,
    ofType r: Type,
    location: LocatableConstraint.Location
  ) {
    let l = typeAssumptions[l]

    // Postpone the solving if `L` is still unknown.
    if case .variable = l {
      postpone(LocatableConstraint(.member(l: l, m: member, r: r), location: location))
      return
    }

    // Search for candidates.
    let matches = checker.lookup(member.stem, memberOf: l, inScope: scope)
    let candidates = matches.compactMap({ (match) -> (decl: AnyDeclID, type: Type)? in
      // Realize the type of the declaration.
      let matchType = checker.realize(decl: match)
      if matchType.isError { return nil }

      // TODO: Handle bound generic typess

      return (decl: match, type: matchType)
    })

    // Rewrite the constraint.
    switch candidates.count {
    case 0:
      diagnostics.append(.undefined(name: "\(member)", range: range(of: location)))

    case 1:
      solve(candidates[0].type, equalsTo: r, location: location)
      if let node = location.node,
         let name = NodeID<NameExpr>(converting: node)
      {
        bindingAssumptions[name] = .member(candidates[0].decl)
      }

    default:
      // TODO: Create an overload constraint
      fatalError("not implemented")
    }
  }

  private mutating func solve(disjunction: LocatableConstraint) -> Solution? {
    guard case .disjunction(let minterms) = disjunction.constraint else { unreachable() }

    var solutions: [Solution] = []
    for minterm in minterms {
      // Don't bother if there's no chance to find a better solution.
      let s = Solution.Score(
        errorCount: diagnostics.count, penalties: penalties + minterm.penalties)
      if s > best { continue }

      // Explore the result of this choice.
      var subsolver = self
      subsolver.penalties += minterm.penalties
      for constraint in minterm.constraints {
        subsolver.fresh.append(LocatableConstraint(constraint, location: disjunction.location))
      }

      guard let solution = subsolver.solve() else { continue }
      if solutions.isEmpty || (solution.score < best) {
        best = solution.score
        solutions = [solution]
      } else if solution.score == best {
        // TODO: Avoid duplicates
        solutions.append(solution)
      }
    }

    switch solutions.count {
    case 0:
      return nil

    case 1:
      return solutions[0]

    default:
      // TODO: Merge remaining solutions
      var s = solutions[0]
      s.diagnostics.append(.ambiguousDisjunction(range: range(of: disjunction.location)))
      return s
    }
  }

  private mutating func postpone(_ constraint: LocatableConstraint) {
    stale.append(constraint)
  }

  /// Returns the source range corresponding to `location`, if any.
  private func range(of location: LocatableConstraint.Location) -> SourceRange? {
    location.node.map({ checker.ast.ranges[$0] }) ?? nil
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
  private func finalize() -> Solution {
    assert(fresh.isEmpty)
    var s = Solution(
      typeAssumptions: typeAssumptions.flattened(),
      bindingAssumptions: bindingAssumptions,
      penalties: penalties,
      diagnostics: diagnostics)

    for c in stale {
      s.diagnostics.append(.staleConstraint(constraint: c.constraint, range: range(of: c.location)))
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

fileprivate protocol LabeledCollection {

  associatedtype Labels: Collection where Labels.Element == String?

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
