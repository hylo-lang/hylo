/// A constraint `L ⤷ R` specifying that `R` is a parameter type and `L` the type of a compatible
/// argument.
///
/// - Note: Solving a constraint `l ⤷ R` where `R` is a type variable requires that there be
///   another constraint on `R` fixing its parameter passing convention.
struct ParameterConstraint: Constraint, Hashable {

  /// The left operand.
  private(set) var left: Type

  /// The right operand.
  private(set) var right: Type

  var cause: ConstraintCause

  /// Creates an instance with the given properties.
  init(_ left: Type, _ right: Type, because cause: ConstraintCause) {
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

extension ParameterConstraint: CustomStringConvertible {

  var description: String { "\(left) ⤷ \(right)" }

}
