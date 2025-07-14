import Utils

/// A constraint `L == R` specifying that `L` is exactly the same type as `R`.
struct EqualityConstraint: Constraint, Hashable, Sendable {

  /// The left operand.
  private(set) var left: AnyType

  /// The right operand.
  private(set) var right: AnyType

  let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  init(_ left: AnyType, _ right: AnyType, origin: ConstraintOrigin) {
    self.left = left
    self.right = right
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

extension EqualityConstraint: CustomStringConvertible {

  var description: String {
    "\(left) == \(right)"
  }

}
