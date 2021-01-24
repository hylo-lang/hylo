import AST

/// A type substitution table.
struct SubstitutionTable {

  init(_ substitutions: [TypeVar: ValType] = [:]) {
    self.substitutions = substitutions
  }

  private var substitutions: [TypeVar: ValType]

  subscript(type: ValType) -> ValType {
    guard let tau = type as? TypeVar else { return type }
    var walked = substitutions[tau]
    while let sigma = walked as? TypeVar {
      walked = substitutions[sigma]
    }
    return walked ?? type
  }

  mutating func substitute(_ type: ValType, for tau: TypeVar) {
    var walked = tau
    while let subst = substitutions[walked] {
      guard let sigma = subst as? TypeVar else {
        precondition(subst == type, "'\(tau)' already substituted for '\(subst)'")
        return
      }
      walked = sigma
    }
    substitutions[walked] = type
  }

  /// Flattens the substitution table, mapping each type variable to their walked assignment.
  func flattened() -> [TypeVar: ValType] {
    return substitutions.mapValues({ self[$0] })
  }

}
