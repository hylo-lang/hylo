/// An associated value declaration.
public struct AssociatedValueDecl: SingleEntityDecl {

  public static let kind = NodeKind.associatedValueDecl

  /// The identifier of the type.
  public var identifier: SourceRepresentable<Identifier>

  /// The where clause of the declaration, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public var defaultValue: AnyExprID?

  public init(
    identifier: SourceRepresentable<Identifier>,
    whereClause: SourceRepresentable<WhereClause>? = nil,
    defaultValue: AnyExprID? = nil
  ) {
    self.identifier = identifier
    self.whereClause = whereClause
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
