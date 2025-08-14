/// A constraint that consists of an arbitrary expression that must evaluate to `true`.
struct PredicateConstraint: Constraint, Hashable, Sendable {

  /// The expression of the constraint.
  let expr: AnyExprID

  let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  init(_ expr: AnyExprID, origin: ConstraintOrigin) {
    self.expr = expr
    self.origin = origin
  }

  /// Inserts the type variables that occur free in `self` into `s`.
  func collectOpenVariables(in s: inout Set<TypeVariable>) {
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
  }

}

extension PredicateConstraint: CustomStringConvertible {

  var description: String { "\(expr)" }

}
