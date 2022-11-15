/// A declaration that extends a type with new members.
public struct ExtensionDecl: TypeExtendingDecl {

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The expression of the extended type.
  public let subject: AnyTypeExprID

  /// The condition of the extension, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the extension.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties and no `accessModifier`.
  public init(
    subject: AnyTypeExprID,
    whereClause: SourceRepresentable<WhereClause>?,
    members: [AnyDeclID]
  ) {
    self.subject = subject
    self.whereClause = whereClause
    self.members = members
  }

  /// Incorportates `accessModifer` into `self`.
  public mutating func incorporate(_ accessModifier: SourceRepresentable<AccessModifier>?) {
    precondition(self.accessModifier == nil)
    self.accessModifier = accessModifier
  }
}
