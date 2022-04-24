import Utils

/// A constraint system solver.
struct ConstraintSolver {

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker!

  /// The scope in which the constraints are solved.
  let scope: AnyScopeID

  /// The fresh constraints to solve.
  var fresh: [LocatableConstraint] = []

  /// The constraints that are currently stale.
  var stale: [LocatableConstraint] = []

  /// The assumptions of the type solver.
  var assumptions = SubstitutionMap()

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
    let l = assumptions[l]

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
    let l = assumptions[l]
    let r = assumptions[r]

    if l == r { return }

    switch (l, r) {
    case (.variable(let tau), _):
      assumptions.assign(r, to: tau)
      refresh(constraintsDependingOn: tau)

    case (_, .variable(let tau)):
      assumptions.assign(l, to: tau)
      refresh(constraintsDependingOn: tau)

    case (.tuple(let l), .tuple(let r)):
      let lLabels = l.elements.map({ $0.label })
      let rLabels = r.elements.map({ $0.label })

      // Make sure both tuple types have a compatible shape.
      if lLabels != rLabels {
        let range = range(of: location)
        if lLabels.count == rLabels.count {
          diagnostics.append(.incompatibleLabels(found: lLabels, expected: rLabels, range: range))
        } else {
          diagnostics.append(.incompatibleTupleLengths(range: range))
        }
        return
      }

      // Break down the constraint.
      for i in 0 ..< l.elements.count {
        fresh.append(LocatableConstraint(
          .equality(l: l.elements[i].type, r: r.elements[i].type), location: location))
      }

    default:
      fatalError("not implemented")
    }
  }

  private mutating func solve(
    _ l: Type,
    isSubtypeOf r: Type,
    location: LocatableConstraint.Location
  ) {
    let l = assumptions[l]
    let r = assumptions[r]

    if l == r { return }

    switch (l, r) {
    case (.tuple, .tuple):
      diagnostics.append(.notSubtype(l, of: r, range: range(of: location)))

    default:
      fatalError("not implemented")
    }
  }

  private mutating func solve(
    _ l: Type,
    passableTo r: Type,
    location: LocatableConstraint.Location
  ) {
    let l = assumptions[l]
    let r = assumptions[r]

    if l == r { return }

    switch r {
    case .variable:
      // Postpone the solving until we can infer the parameter passing convention of `R`.
      postpone(LocatableConstraint(.parameter(l: l, r: r), location: location))

    case .parameter(let p):
      // Either `L` is equal to the bare type of `R`, or it's a. Note: the equality requirement for
      // arguments passed mutably is verified after type inference.
      fresh.append(LocatableConstraint(
        .disjunction([
          Constraint.Minterm(constraints: [.equality(l: l, r: p.bareType)], penalties: 0),
          Constraint.Minterm(constraints: [.subtyping(l: l, r: p.bareType)], penalties: 1),
        ]),
        location: location))

    default:
      diagnostics.append(.invalidParameterType(r, range: range(of: location)))
    }
  }

  private mutating func solve(disjunction: LocatableConstraint) -> Solution {
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
      if solution.score < best {
        best = solution.score
        solutions = [solution]
      } else if solution.score == best {
        // TODO: Avoid duplicates
        solutions.append(solution)
      }
    }

    assert(!solutions.isEmpty)
    if solutions.count == 1 {
      return solutions[0]
    } else {
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
      assumptions: assumptions.flattened(),
      penalties: penalties,
      diagnostics: diagnostics)

    for c in stale {
      s.diagnostics.append(.staleConstraint(constraint: c.constraint, range: range(of: c.location)))
    }

    return s
  }

}
