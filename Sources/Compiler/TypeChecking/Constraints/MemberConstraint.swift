/// A constraint `L.m == R` specifying that `L` has a member of type `R` named `m`.
struct MemberConstraint: Constraint, Hashable {

  /// The base type of the left operand.
  private(set) var subject: AnyType

  /// The expression referring to `subject`'s member in the AST, if available.
  let memberExpr: NodeID<NameExpr>?

  /// The name of the member in `subject` that must have type `memberType`.
  let memberName: Name

  /// The right operand.
  private(set) var memberType: AnyType

  var cause: ConstraintCause

  /// Creates a constraint requiring `subject` to have a member of type `memberType` and whose
  /// name is expressed by `memberExpr` in the AST.
  init(
    _ subject: AnyType,
    hasMemberReferredToBy memberExpr: NodeID<NameExpr>,
    ofType member: AnyType,
    in ast: AST,
    because cause: ConstraintCause
  ) {
    self.subject = subject
    self.memberExpr = memberExpr
    self.memberName = ast[memberExpr].name.value
    self.memberType = member
    self.cause = cause
  }

  mutating func modifyTypes(_ modify: (inout AnyType) -> Void) {
    modify(&subject)
    modify(&memberType)
  }

  func depends(on variable: TypeVariable) -> Bool {
    (subject == variable) || (memberType == variable)
  }

}

extension MemberConstraint: CustomStringConvertible {

  var description: String { "\(subject).\(memberName) == \(memberType)" }

}
