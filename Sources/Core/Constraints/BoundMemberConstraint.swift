/// A constraint `L.m == R` specifying that `L` has a member of type `R` named `m`.
public struct MemberConstraint: Constraint, Hashable {

  /// The base type of the left operand.
  public private(set) var left: AnyType

  /// The right operand.
  public private(set) var right: AnyType

  /// The name of the member in `left` that must have type `right`.
  public let member: Name

  /// The expression of `member` in the AST, if available.
  public let memberExpr: NodeID<NameExpr>?

  public var cause: ConstraintCause

  /// Creates an instance with the given properties.
  public init(
    _ left: AnyType,
    hasMember member: Name,
    ofType right: AnyType,
    cause: ConstraintCause
  ) {
    self.left = left
    self.right = right
    self.member = member
    self.memberExpr = nil
    self.cause = cause
  }

  /// Creates an instance with the given properties.
  public init(
    _ left: AnyType,
    hasMemberExpressedBy memberExpr: NodeID<NameExpr>,
    ofType right: AnyType,
    in ast: AST,
    cause: ConstraintCause
  ) {
    self.left = left
    self.right = right
    self.member = ast[memberExpr].name.value
    self.memberExpr = memberExpr
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

extension MemberConstraint: CustomStringConvertible {

  public var description: String { "bound(\(left).\(member)) == \(right)" }

}
