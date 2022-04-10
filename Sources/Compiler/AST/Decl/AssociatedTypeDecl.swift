/// An associated type declaration.
public struct AssociatedTypeDecl: SingleEntityDecl {

  /// The identifier of the type.
  public var identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public var conformances: [NodeIndex<NameTypeExpr>]

  /// The where clause of the declaration, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public var defaultValue: AnyTypeExprIndex?

  public var name: String { identifier.value }

}
