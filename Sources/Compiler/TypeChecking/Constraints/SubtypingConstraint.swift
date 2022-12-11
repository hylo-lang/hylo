/// A constraint `L <: R` specifying that `L` is a subtype of `R`.
struct SubtypingConstraint: Constraint, Hashable {

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

  mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&left)
    modify(&right)
  }

  func depends(on variable: TypeVariable) -> Bool { (left == variable) || (right == variable) }

}

extension SubtypingConstraint: CustomStringConvertible {

  var description: String { "\(left) <: \(right)" }

}
