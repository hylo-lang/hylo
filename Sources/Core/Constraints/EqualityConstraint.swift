import Utils

/// A constraint `L == R` specifying that `L` is exactly the same type as `R`.
///
/// - Note: Equality constraints are commutative.
public struct EqualityConstraint: Constraint, Hashable {

  /// The left operand.
  public private(set) var left: AnyType

  /// The right operand.
  public private(set) var right: AnyType

  public let cause: ConstraintCause

  /// Creates an instance with the given properties.
  public init(_ left: AnyType, _ right: AnyType, because cause: ConstraintCause) {
    self.left = left
    self.right = right
    self.cause = cause
  }

  /// Creates an instance transforming by `constraint`.
  public init(_ constraint: SubtypingConstraint) {
    self.left = constraint.left
    self.right = constraint.right
    self.cause = constraint.cause
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    modify(&left, with: transform)
    modify(&right, with: transform)
  }

  public func depends(on variable: TypeVariable) -> Bool {
    (left == variable) || (right == variable)
  }

}

extension EqualityConstraint: CustomStringConvertible {

  public var description: String { "\(left) == \(right)" }

}
