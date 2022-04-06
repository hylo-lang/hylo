/// An associated type declaration.
public struct AssociatedTypeDecl: Decl, SourceRepresentable {

  public var range: SourceRange?

  /// The static modifier of the declaration, if any.
  public var staticModifier: StaticModifier?

  /// The conformances listed in the declaration.
  public var conformances: [NameTypeExpr]

  /// The where clause of the declaration, if any.
  public var whereClause: WhereClause?

  /// The default value of the declaration, if any.
  public var defaultValue: TypeExpr?

}
