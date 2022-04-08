/// The declaration of a conformance.
public struct ConformanceDecl: Decl, ScopeOutliner {

  var scopeID: ScopeID

  /// The expression of the conforming type.
  public var subject: SourceRepresentable<TypeExpr>

  /// The names of traits to which conformance is declared.
  public var conformances: [SourceRepresentable<NameTypeExpr>]

  /// The condition of the conformance, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the conformance.
  public var members: [AnyDeclIndex]

  public var range: SourceRange?

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(conformance: self)
  }

}
