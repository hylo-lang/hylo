/// The declaration of an extension.
public struct ExtensionDecl: Decl, LexicalScope {

  public static let kind = NodeKind.extensionDecl

  /// The expression of the extended type.
  public var subject: AnyTypeExprIndex

  /// The condition of the extension, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the extension.
  public var members: [AnyDeclIndex]

}
