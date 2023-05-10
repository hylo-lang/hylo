import Core
import Utils

/// A constraint `L == R` specifying that `L` is exactly the same type as `R`.
public struct EqualityConstraint: Constraint, Hashable {

  /// The left operand.
  public private(set) var left: AnyType

  /// The right operand.
  public private(set) var right: AnyType

  public let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  public init(_ left: AnyType, _ right: AnyType, origin: ConstraintOrigin) {
    self.left = left
    self.right = right
    self.origin = origin
  }

  /// Creates an instance transforming by `constraint`.
  public init(_ constraint: SubtypingConstraint) {
    self.left = constraint.left
    self.right = constraint.right
    self.origin = constraint.origin
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&left, with: transform)
    update(&right, with: transform)
  }

}

extension EqualityConstraint: CustomStringConvertible {

  public var description: String { "\(left) == \(right)" }

}
