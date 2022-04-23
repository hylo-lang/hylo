/// An error encountered during type checking.
struct TypeError {

  /// The kind of a type error.
  enum Kind {

    /// The solver could not choose how to solve the associated disjunction constraint.
    ///
    /// - Requires: The associated constraint must be a disjunction.
    case ambiguousDisjunction

    /// The type does not conform to `traits` in `scope`.
    case doesNotConform(Type, traits: Set<TraitType>, scope: AnyScopeID)

    /// The right hand side of a parameter constraint is not a parameter type.
    case nonParameterType(Type)

    /// The associated constraint went stale.
    case staleConstaint

  }

  /// The kind of the error.
  var kind: Kind

  /// The constraint that caused the type error.
  var cause: LocatableConstraint

  /// Returns the diagnostics of this error.
  func diagnose(ast: AST) -> [Diagnostic] {
    // Identify the source range of the error.
    let range = cause.node.map({ ast.ranges[$0] }) ?? nil

    switch kind {
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
