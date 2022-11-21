/// A constraint that consists of an arbitrary expression that must evaluate to `true`.
struct PredicateConstraint: Constraint, Hashable {

  /// The expression of the constraint.
  let expr: AnyExprID

  var cause: ConstraintCause?

  /// Creates an instance with the given properties.
  init(_ expr: AnyExprID, because cause: ConstraintCause? = nil) {
    self.expr = expr
    self.cause = cause
  }

  func modifyTypes(_ modify: (inout Type) -> Void) {}

  func depends(on variable: TypeVariable) -> Bool { false }

}

extension PredicateConstraint: CustomStringConvertible {

  var description: String { "\(expr)" }

}
