/// A constraint `unbound(L.m) == R` specifying that `L` has a static or non-static member of
/// type `R` named `m` and  referred to from an unbound context.
struct UnboundMemberConstraint: Constraint, Hashable {

  /// The base type of the left operand.
  private(set) var left: Type

  /// The right operand.
  private(set) var right: Type

  /// The name of the member in `left` that must have type `right`.
  let member: Name

  /// The expression of `member` in the AST, if available.
  let memberExpr: NodeID<NameExpr>?

  var cause: ConstraintCause?

  /// Creates an instance with the given properties.
  init(
    type left: Type,
    hasMemberNamed member: Name,
    ofType right: Type,
    because cause: ConstraintCause? = nil
  ) {
    self.left = left
    self.right = right
    self.member = member
    self.memberExpr = nil
    self.cause = cause
  }

  /// Creates an instance with the given properties.
  init(
    type left: Type,
    hasMemberExpressedBy memberExpr: NodeID<NameExpr>,
    in ast: AST,
    ofType right: Type,
    because cause: ConstraintCause? = nil
  ) {
    self.left = left
    self.right = right
    self.member = ast[memberExpr].name.value
    self.memberExpr = memberExpr
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
