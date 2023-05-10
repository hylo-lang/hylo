import Core

/// A constraint that consists of an arbitrary expression that must evaluate to `true`.
struct PredicateConstraint: Constraint, Hashable {

  /// The expression of the constraint.
  let expr: AnyExprID

  let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  init(_ expr: AnyExprID, origin: ConstraintOrigin) {
    self.expr = expr
    self.origin = origin
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {}

}

extension PredicateConstraint: CustomStringConvertible {

  var description: String { "\(expr)" }

}
