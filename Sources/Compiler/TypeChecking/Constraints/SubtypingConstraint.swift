/// A constraint `L <: R` specifying that `L` is a subtype of `R`.
struct SubtypingConstraint: Constraint, Hashable {

  /// The left operand.
  private(set) var left: Type

  /// The right operand.
  private(set) var right: Type

  var cause: ConstraintCause?

  /// Creates an instance with the given properties.
  init(_ left: Type, _ right: Type, because cause: ConstraintCause? = nil) {
    self.left = left
    self.right = right
    self.cause = cause
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

extension SubtypingConstraint: CustomStringConvertible {

  var description: String { "\(left) <: \(right)" }

}
