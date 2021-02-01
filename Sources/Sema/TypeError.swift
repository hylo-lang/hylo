import AST

/// A type error encountered by a constraint solver.
enum TypeError {

  case conflictingTypes(RelationalConstraint)

  case conflictingLabels(RelationalConstraint)

  case nonExistentProperty(Constraint)

  case nonConformingType(Constraint)

  case ambiguousConstraint(Constraint)

  case noViableOverload(OverloadBindingConstraint)

  case multipleOverloads(OverloadBindingConstraint, [Int])

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
    switch (lhs.constraint.locator, rhs.constraint.locator) {
    case (.some(let a), .some(let b)):
      return a.anchor.range.lowerBound < b.anchor.range.lowerBound
    case (.some, _):
      return true
    case (nil, _):
      return false
    }
  }

}
