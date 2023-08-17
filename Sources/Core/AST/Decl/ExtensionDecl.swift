/// A declaration that extends a type with new members.
public struct ExtensionDecl: TypeExtendingDecl {

  public static let constructDescription = "extension declaration"

  public let site: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The expression of the extended type.
  public let subject: AnyExprID

  /// The condition of the extension, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the extension.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>,
    subject: AnyExprID,
    whereClause: SourceRepresentable<WhereClause>?,
    members: [AnyDeclID],
    site: SourceRange
  ) {
    self.site = site
    self.accessModifier = accessModifier
    self.subject = subject
    self.whereClause = whereClause
    self.members = members
  }

}
