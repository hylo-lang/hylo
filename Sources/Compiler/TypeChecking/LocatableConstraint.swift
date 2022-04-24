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

    case callArgument

  }

  /// The constraint.
  var constraint: Constraint

  /// The location of the constraint.
  var location: Location

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
