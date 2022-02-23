/// A type substitution table.
struct SubstitutionTable {

  init(_ substitutions: [TypeVar: ValType] = [:]) {
    self.substitutions = substitutions
  }

  private var substitutions: [TypeVar: ValType]

  subscript(type: ValType) -> ValType {
    var walked = type
    while let t = walked as? TypeVar {
      if let u = substitutions[t] {
        walked = u
      } else {
        break
      }
    }
    return walked
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
