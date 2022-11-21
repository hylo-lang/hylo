/// A constraint `unbound(L.m) == R` specifying that `L` has a static or non-static member of
/// type `R` named `m` and  referred to from an unbound context.
struct UnboundMemberConstraint: Constraint, Hashable {

  /// The left operand.
  private(set) var left: Type

  /// The name of the member in `left` that must have type `right`.
  let member: Name

  /// The right operand.
  private(set) var right: Type

  var cause: ConstraintCause?

  /// Creates an instance with the given properties.
  init(
    _ left: Type,
    hasMemberNamed member: Name,
    ofType right: Type,
    because cause: ConstraintCause? = nil
  ) {
    self.left = left
    self.member = member
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

extension UnboundMemberConstraint: CustomStringConvertible {

  var description: String { "unbound(\(left).\(member)) == \(right)" }

}
