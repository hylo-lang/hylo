/// A declaration that extends a type with new members.
public struct ExtensionDecl: TypeExtendingDecl, GenericScope {

  public static let kind = NodeKind.extensionDecl

  /// The expression of the extended type.
  public var subject: AnyTypeExprID

  /// The condition of the extension, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the extension.
  public var members: [AnyDeclID]

  public init(
    subject: AnyTypeExprID,
    whereClause: SourceRepresentable<WhereClause>? = nil,
    members: [AnyDeclID] = []
  ) {
    self.subject = subject
    self.whereClause = whereClause
    self.members = members
  }

}
