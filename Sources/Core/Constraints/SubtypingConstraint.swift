/// A constraint `L <: R` specifying that `L` is a subtype of `R`.
public struct SubtypingConstraint: Constraint, Hashable {

  /// The left operand.
  public private(set) var left: AnyType

  /// The right operand.
  public private(set) var right: AnyType

  public var cause: ConstraintCause

  /// Creates an instance with the given properties.
  public init(_ left: AnyType, _ right: AnyType, because cause: ConstraintCause) {
    self.left = left
    self.right = right
    self.cause = cause
  }

  public mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&left)
    modify(&right)
  }

  public func depends(on variable: TypeVariable) -> Bool {
    (left == variable) || (right == variable)
  }

}

extension SubtypingConstraint: CustomStringConvertible {

  public var description: String { "\(left) <: \(right)" }

}
