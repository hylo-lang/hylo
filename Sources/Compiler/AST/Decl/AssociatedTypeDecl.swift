/// An associated type declaration.
public struct AssociatedTypeDecl: SingleEntityDecl {

  public static let kind = NodeKind.associatedTypeDecl

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public let conformances: [NodeID<NameTypeExpr>]

  /// The where clause of the declaration, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public let defaultValue: AnyTypeExprID?

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

  public var name: String { identifier.value }

}
