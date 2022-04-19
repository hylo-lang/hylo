/// An error encountered during type checking.
struct TypeError {

  /// The kind of a type error.
  enum Kind {

    /// The type does not conform to `traits` in `scope`.
    case doesNotConform(Type, traits: Set<TraitType>, scope: AnyScopeID)

    /// The associated constraint went stale.
    case staleConstaint

  }

  /// The kind of the error.
  var kind: Kind

  /// The constraint that cause the type error.
  var cause: LocatableConstraint

  /// Returns the diagnostics of this error.
  func diagnose(ast: AST) -> [Diagnostic] {
    // Identify the source range of the error.
    let range = cause.node.map({ ast.ranges[$0] }) ?? nil

    switch kind {
    case .doesNotConform(let type, let traits, _):
      return traits.map({ trait in
        Diagnostic.noConformance(of: type, to: trait, range: range)
      })

    case .staleConstaint:
      return [.staleConstraint(constraint: cause.constraint, range: range)]
    }
  }

}
