/// A constraint `L ⤷ R` specifying that `R` is a parameter type and `L` the type of a compatible
/// argument.
///
/// - Note: Solving a constraint `l ⤷ R` where `R` is a type variable requires that there be
///   another constraint on `R` fixing its parameter passing convention.
public struct ParameterConstraint: Constraint, Hashable {

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

  public mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&left)
    modify(&right)
  }

  public func depends(on variable: TypeVariable) -> Bool {
    (left == variable) || (right == variable)
  }

}

extension ParameterConstraint: CustomStringConvertible {

  public var description: String { "\(left) ⤷ \(right)" }

}
