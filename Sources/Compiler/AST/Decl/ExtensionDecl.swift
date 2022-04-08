/// The declaration of an extension.
public struct ExtensionDecl: Decl, ScopeOutliner {

  var scopeID: ScopeID

  /// The expression of the extended type.
  public var subject: SourceRepresentable<TypeExpr>

  /// The condition of the extension, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the extension.
  public var members: [AnyDeclIndex]

  public var range: SourceRange?

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(extension: self)
  }

}
