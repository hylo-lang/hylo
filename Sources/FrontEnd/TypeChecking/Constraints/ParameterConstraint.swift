import Core
import Utils

/// A constraint `L ⤷ R` specifying that `R` is the type of a parameter and `L` the type of an
/// argument that may be passed to that parameter.
///
/// - Note: Solving a constraint `l ⤷ R` where `R` is a type variable requires that there be
///   another constraint on `R` fixing its parameter passing convention.
struct ParameterConstraint: Constraint, Hashable {

  /// The left operand.
  private(set) var left: AnyType

  /// The right operand.
  private(set) var right: AnyType

  /// The right operand.
  private(set) var argument: AnyExprID

  let origin: ConstraintOrigin

  /// Creates an instance with the given properties.
  init(_ left: AnyType, _ right: AnyType, origin: ConstraintOrigin, withArgument e: AnyExprID) {
    self.left = left
    self.right = right
    self.origin = origin
    self.argument = e
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

extension ParameterConstraint: CustomStringConvertible {

  var description: String { "\(left) (expr: \(argument)) ⤷ \(right)" }

}
