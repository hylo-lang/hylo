/// The declaration of an extension.
public struct ExtensionDecl: Decl, SourceRepresentable {

  public var range: SourceRange?

  /// The expression of the extended type.
  public var subject: TypeExpr

  /// The condition of the extension, if any.
  public var whereClause: WhereClause?

  /// The member declarations in the lexical scope of the extension.
  public var members: [AnyDeclIndex]

}
