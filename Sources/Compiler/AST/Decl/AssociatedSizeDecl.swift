/// An associated size declaration.
public struct AssociatedSizeDecl: SingleEntityDecl {

  public static let kind = NodeKind.associatedSizeDecl

  /// The identifier of the type.
  public var identifier: SourceRepresentable<Identifier>

  /// The where clause of the declaration, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public var defaultValue: AnyExprIndex?

  public var name: String { identifier.value }

}
