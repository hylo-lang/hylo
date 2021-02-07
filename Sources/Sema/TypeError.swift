import AST

/// A type error encountered by a constraint solver.
enum TypeError {

  case conflictingTypes(RelationalConstraint)

  case conflictingLabels(RelationalConstraint)

  case nonConformingType(RelationalConstraint)

  case nonExistentProperty(Constraint)

  case ambiguousConstraint(Constraint)

  case noViableOverload(OverloadBindingConstraint)

  case multipleOverloads(OverloadBindingConstraint, [ValueDecl])

  var constraint: Constraint {
    switch self {
    case .conflictingTypes    (let c)   : return c
    case .conflictingLabels   (let c)   : return c
    case .nonExistentProperty (let c)   : return c
    case .nonConformingType   (let c)   : return c
    case .ambiguousConstraint (let c)   : return c
    case .noViableOverload    (let c)   : return c
    case .multipleOverloads   (let c, _): return c
    }
  }

  static func < (_ lhs: TypeError, _ rhs: TypeError) -> Bool {
    let a = lhs.constraint.locator.anchor.range.lowerBound
    let b = rhs.constraint.locator.anchor.range.lowerBound
    return a < b
  }

}
