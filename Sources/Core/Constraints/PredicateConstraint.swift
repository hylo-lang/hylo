/// A constraint that consists of an arbitrary expression that must evaluate to `true`.
public struct PredicateConstraint: Constraint, Hashable {

  /// The expression of the constraint.
  let expr: AnyExprID

  public let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  public init(_ expr: AnyExprID, origin: ConstraintOrigin) {
    self.expr = expr
    self.origin = origin
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {}

}

extension PredicateConstraint: CustomStringConvertible {

  public var description: String { "\(expr)" }

}
