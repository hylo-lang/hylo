import Core
import Utils

/// A constraint `(L::P).m == R` specifying that `L` has an associated required by trait `P` and
/// implemented as `R`.
struct AssociatedMemberConstraint: Constraint, Hashable {

  /// The left operand.
  private(set) var subject: AnyType

  /// The right operand.
  private(set) var memberType: AnyType

  /// The declaration of the associated requirement.
  let memberDecl: AssociatedTypeDecl.ID

  /// The name of the associated requirement.
  let memberName: String

  /// The site from which a constraint originates and the reason why it was formed.
  let origin: ConstraintOrigin

  /// Creates a constraint requiring `s` to have an associated type `m`, declared by `d` in `ast`.
  init(
    _ s: AnyType, hasAssociatedMember m: AnyType, declaredBy d: AssociatedTypeDecl.ID, in ast: AST,
    origin: ConstraintOrigin
  ) {
    self.subject = s
    self.memberType = m
    self.memberDecl = d
    self.memberName = ast[d].baseName
    self.origin = origin
  }

  /// Inserts the type variables that occur free in `self` into `s`.
  func collectOpenVariables(in s: inout Set<TypeVariable>) {
    subject.collectOpenVariables(in: &s)
    memberType.collectOpenVariables(in: &s)
  }

  mutating func modifyTypes(_ transform: (AnyType) -> AnyType) {
    update(&subject, with: transform)
    update(&memberType, with: transform)
  }

}

extension AssociatedMemberConstraint: CustomStringConvertible {

  var description: String { "\(subject).\(memberName) == \(memberType)" }

}

