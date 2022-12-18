/// A constraint that consists of an arbitrary expression that must evaluate to `true`.
public struct PredicateConstraint: Constraint, Hashable {

  /// The expression of the constraint.
  let expr: AnyExprID

  public let cause: ConstraintCause

  /// Creates an instance with the given properties.
  public init(_ expr: AnyExprID, because cause: ConstraintCause) {
    self.expr = expr
    self.cause = cause
  }

  public func modifyTypes(_ modify: (inout AnyType) -> Void) {}

  public func depends(on variable: TypeVariable) -> Bool { false }

}

extension PredicateConstraint: CustomStringConvertible {

  public var description: String { "\(expr)" }

}
