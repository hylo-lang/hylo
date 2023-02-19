/// A constraint that consists of an arbitrary expression that must evaluate to `true`.
public struct PredicateConstraint: Constraint, Hashable {

  /// The expression of the constraint.
  let expr: AnyExprID

  public let cause: ConstraintOrigin

  /// Creates an instance with the given properties.
  public init(_ expr: AnyExprID, because cause: ConstraintOrigin) {
    self.expr = expr
    self.cause = cause
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {}

}

extension PredicateConstraint: CustomStringConvertible {

  public var description: String { "\(expr)" }

}
