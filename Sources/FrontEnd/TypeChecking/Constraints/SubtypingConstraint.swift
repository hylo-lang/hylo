import Utils

/// A constraint `L <: R` (or `L < R`) specifying that `L` is subtype (or strict subtype) of `R`.
///
/// - Warning: should not be used directly for inference purposes or the type checker will get
///   stuck. Use `inferenceConstraint(_:isSubtypeOf:origin:)` instead.
struct SubtypingConstraint: Constraint, Hashable, Sendable {

  /// The left operand.
  private(set) var left: AnyType

  /// The right operand.
  private(set) var right: AnyType

  /// Indicates whether `left` must be a strict subtype of `right`.
  let isStrict: Bool

  let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  init(
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

  /// Inserts the type variables that occur free in `self` into `s`.
  func collectOpenVariables(in s: inout Set<TypeVariable>) {
    left.collectOpenVariables(in: &s)
    right.collectOpenVariables(in: &s)
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&left, with: transform)
    update(&right, with: transform)
  }

}

extension SubtypingConstraint: CustomStringConvertible {

  var description: String {
    isStrict ? "\(left) < \(right)" : "\(left) <: \(right)"
  }

}
