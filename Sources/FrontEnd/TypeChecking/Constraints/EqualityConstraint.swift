/// A constraint `L == R` specifying that `L` is exactly the same type as `R`.
///
/// - Note: Equality constraints are commutative.
struct EqualityConstraint: Constraint, Hashable {

  /// The left operand.
  private(set) var left: AnyType

  /// The right operand.
  private(set) var right: AnyType

  var cause: ConstraintCause

  /// Creates an instance with the given properties.
  init(_ left: AnyType, _ right: AnyType, because cause: ConstraintCause) {
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

  mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&left)
    modify(&right)
  }

  func depends(on variable: TypeVariable) -> Bool {
    (left == variable) || (right == variable)
  }

}

extension EqualityConstraint: CustomStringConvertible {

  public var description: String { "\(left) == \(right)" }

}
