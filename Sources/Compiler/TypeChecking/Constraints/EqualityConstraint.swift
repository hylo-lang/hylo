/// A constraint `L == R` specifying that `L` is exactly the same type as `R`.
///
/// - Note: Equality constraints are commutative.
struct EqualityConstraint: Constraint, Hashable {

  /// The left operand.
  private(set) var left: Type

  /// The right operand.
  private(set) var right: Type

  var cause: ConstraintCause

  /// Creates an instance with the given properties.
  init(_ left: Type, _ right: Type, because cause: ConstraintCause) {
    self.left = left
    self.right = right
    self.cause = cause
  }

  /// Creates an instance transforming by `constraint`.
  init(_ constraint: SubtypingConstraint) {
    self.left = constraint.left
    self.right = constraint.right
    self.cause = constraint.cause
  }

  mutating func modifyTypes(_ modify: (inout Type) -> Void) {
    modify(&left)
    modify(&right)
  }

  func depends(on variable: TypeVariable) -> Bool {
    let v = Type.variable(variable)
    return (left == v) || (right == v)
  }

}

extension EqualityConstraint: CustomStringConvertible {

  public var description: String { "\(left) == \(right)" }

}
