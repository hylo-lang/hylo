/// A constraint whose origin can be located in the source code.
struct LocatableConstraint {

  /// The cause of a locatable constraint.
  enum Cause {

    case annotation

  }

  /// The constraint.
  var constraint: Constraint

  /// The AST node to which the constraint is attached, if any.
  var node: AnyNodeID?

  /// The cause of the constraint, if specified.
  var cause: Cause?

  init(_ constraint: Constraint, node: AnyNodeID? = nil, cause: Cause? = nil) {
    self.constraint = constraint
    self.node = node
    self.cause = cause
  }

}
