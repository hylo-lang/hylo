/// An error encountered during type checking.
enum TypeError {

  /// The solver could not choose how to solve the a disjunction constraint.
  ///
  /// - Requires: The associated constraint must be a disjunction.
  case ambiguousDisjunction

  /// The type does not conform to `traits` in `scope`.
  case doesNotConform(Type, traits: Set<TraitType>, scope: AnyScopeID)

  /// The right hand side of a parameter constraint is not a parameter type.
  case nonParameterType(Type)

  /// A constraint went stale.
  case staleConstaint

  /// Returns diagnostics of this error.
  func diagnose(cause: LocatableConstraint, ast: AST) -> [Diagnostic] {
    // Identify the source range of the error.
    let range = cause.node.map({ ast.ranges[$0] }) ?? nil

    switch self {
    case .ambiguousDisjunction:
      fatalError("not implemented")

    case .doesNotConform(let type, let traits, _):
      return traits.map({ trait in
        Diagnostic.noConformance(of: type, to: trait, range: range)
      })

    case .nonParameterType(_):
      fatalError("not implemented")

    case .staleConstaint:
      return [.staleConstraint(constraint: cause.constraint, range: range)]
    }
  }

}
