import Core
import Utils

/// A constraint `L.m == R` specifying that `L` has a member of type `R` named `m`.
struct MemberConstraint: Constraint, Hashable {

  /// The base type of the left operand.
  private(set) var subject: AnyType

  /// The right operand.
  private(set) var memberType: AnyType

  /// The expression referring to `subject`'s member.
  let memberExpr: NameExpr.ID

  /// The name of the member in `subject` that must have type `memberType`.
  let memberName: Name

  /// The purpose of `memberExpr`.
  let purpose: NameUse

  /// The site from which a constraint originates and the reason why it was formed.
  let origin: ConstraintOrigin

  /// Creates a constraint requiring `subject` to have a member of type `memberType` and whose name
  /// is expressed by `memberExpr`.
  ///
  /// - Requires: `memberExpr`'s domain is not `.none`.
  init(
    _ subject: AnyType,
    hasMember memberType: AnyType,
    referredToBy memberExpr: NameExpr.ID,
    in ast: AST,
    usedAs purpose: NameUse,
    origin: ConstraintOrigin
  ) {
    self.subject = subject
    self.memberType = memberType
    self.memberExpr = memberExpr
    self.memberName = ast[memberExpr].name.value
    self.purpose = purpose
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
