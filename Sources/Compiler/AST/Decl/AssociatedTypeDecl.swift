/// An associated type declaration.
public struct AssociatedTypeDecl: SingleEntityDecl {

  public static let kind = NodeKind.associatedTypeDecl

  /// The identifier of the type.
  public var identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public var conformances: [NodeID<NameTypeExpr>]

  /// The where clause of the declaration, if any.
  public var whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public var defaultValue: AnyTypeExprID?

  public var name: String { identifier.value }

  public init(
    identifier: SourceRepresentable<Identifier>,
    conformances: [NodeID<NameTypeExpr>] = [],
    whereClause: SourceRepresentable<WhereClause>? = nil,
    defaultValue: AnyTypeExprID? = nil
  ) {
    self.identifier = identifier
    self.conformances = conformances
    self.whereClause = whereClause
    self.defaultValue = defaultValue
  }

}
