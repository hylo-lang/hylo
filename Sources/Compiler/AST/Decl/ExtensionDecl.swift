/// The declaration of an extension.
public struct ExtensionDecl: Decl, ScopeOutliner, SourceRepresentable {

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The expression of the extended type.
  public var subject: TypeExpr

  /// The condition of the extension, if any.
  public var whereClause: WhereClause?

  /// The member declarations in the lexical scope of the extension.
  public var members: [AnyDeclIndex]

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(extension: self)
  }

}
