import Utils

/// A constraint `L <: R` (or `L < R`) specifying that `L` is subtype (or strict subtype) of `R`.
///
/// - Warning: should not be used directly for inference purposes or the type checker will get
///   stuck. Use `inferenceConstraint(_:isSubtypeOf:because:)` instead.
public struct SubtypingConstraint: Constraint, Hashable {

  /// The left operand.
  public private(set) var left: AnyType

  /// The right operand.
  public private(set) var right: AnyType

  /// Indicates whether `left` must be a strict subtype of `right`.
  public let isStrict: Bool

  public let cause: ConstraintOrigin

  /// Creates an instance with the given properties.
  public init(
    _ left: AnyType,
    _ right: AnyType,
    strictly isStrict: Bool = false,
    because cause: ConstraintOrigin
  ) {
    self.left = left
    self.right = right
    self.isStrict = isStrict
    self.cause = cause
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    modify(&left, with: transform)
    modify(&right, with: transform)
  }

}

extension SubtypingConstraint: CustomStringConvertible {

  public var description: String {
    isStrict ? "\(left) < \(right)" : "\(left) <: \(right)"
  }

}
