import Utils

/// A constraint `L <: R` specifying that `L` is a subtype of `R`.
///
/// - Warning: should not be used directly for inference purposes or the type checker will get
///   stuck. Use `inferenceConstraint(_:isSubtypeOf:because:)` instead.
public struct SubtypingConstraint: Constraint, Hashable {

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

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    modify(&left, with: transform)
    modify(&right, with: transform)
  }

}

extension SubtypingConstraint: CustomStringConvertible {

  public var description: String { "\(left) <: \(right)" }

}
