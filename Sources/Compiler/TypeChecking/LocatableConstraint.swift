/// A constraint whose origin can be located in the source code.
struct LocatableConstraint {

  /// An object describing the location of a constraint.
  struct Location {

    /// The AST node to which the constraint is attached, if any.
    var node: AnyNodeID?

    /// The cause of the constraint, if specified.
    var cause: Cause?

  }

  /// The cause of a locatable constraint.
  enum Cause {

    case annotation

    case assignment

    case callArgument

    case cast

    case initialization

    case member

    case `return`

    case `yield`

  }

  /// The constraint.
  private(set) var constraint: Constraint

  /// The location of the constraint.
  private(set) var location: Location

  init(_ constraint: Constraint, location: Location) {
    self.constraint = constraint
    self.location = location
  }

  init(_ constraint: Constraint, node: AnyNodeID? = nil, cause: Cause? = nil) {
    self.constraint = constraint
    self.location = Location(node: node, cause: cause)
  }

}

extension LocatableConstraint: CustomStringConvertible {

  var description: String { "\(constraint)" }

}
