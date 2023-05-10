import Core
import Utils

/// A constraint `L.m == R` specifying that `L` has a member of type `R` named `m`.
struct MemberConstraint: Constraint, Hashable {

  /// The base type of the left operand.
  private(set) var subject: AnyType

  /// The expression referring to `subject`'s member in the AST.
  let memberExpr: NameExpr.ID

  /// The name of the member in `subject` that must have type `memberType`.
  let memberName: Name

  /// The right operand.
  private(set) var memberType: AnyType

  let origin: ConstraintOrigin

  /// Creates a constraint requiring `subject` to have a member of type `memberType` and whose
  /// name is expressed by `memberExpr` in the AST.
  init(
    _ subject: AnyType,
    hasMemberReferredToBy memberExpr: NameExpr.ID,
    ofType member: AnyType,
    in ast: AST,
    origin: ConstraintOrigin
  ) {
    self.subject = subject
    self.memberExpr = memberExpr
    self.memberName = ast[memberExpr].name.value
    self.memberType = member
    self.origin = origin
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&subject, with: transform)
    update(&memberType, with: transform)
  }

}

extension MemberConstraint: CustomStringConvertible {

  var description: String { "\(subject).\(memberName) == \(memberType)" }

}
