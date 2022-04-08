/// An associated type declaration.
public struct AssociatedTypeDecl: Decl {

  /// The static modifier of the declaration, if any.
  public var staticModifier: SourceRepresentable<StaticModifier>?

  /// The conformances listed in the declaration.
  public var conformances: [SourceRepresentable<NameTypeExpr>]

  /// The where clause of the declaration, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public var defaultValue: SourceRepresentable<TypeExpr>?

  public var range: SourceRange?

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(associatedType: self)
  }

}
