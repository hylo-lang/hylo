import Utils

/// A constraint `L <: R` (or `L < R`) specifying that `L` is subtype (or strict subtype) of `R`.
///
/// - Warning: should not be used directly for inference purposes or the type checker will get
///   stuck. Use `inferenceConstraint(_:isSubtypeOf:origin:)` instead.
public struct SubtypingConstraint: Constraint, Hashable {

  /// The left operand.
  public private(set) var left: AnyType

  /// The right operand.
  public private(set) var right: AnyType

  /// Indicates whether `left` must be a strict subtype of `right`.
  public let isStrict: Bool

  public let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  public init(
    _ left: AnyType,
    _ right: AnyType,
    strictly isStrict: Bool = false,
    origin: ConstraintOrigin
  ) {
    self.left = left
    self.right = right
    self.isStrict = isStrict
    self.origin = origin
  }

  public mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    assign(&left, to: transform)
    assign(&right, to: transform)
  }

}

extension SubtypingConstraint: CustomStringConvertible {

  public var description: String {
    isStrict ? "\(left) < \(right)" : "\(left) <: \(right)"
  }

}
