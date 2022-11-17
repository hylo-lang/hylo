
public protocol TypeDecl: SingleEntityDecl {}

/// An associated type declaration.
public struct AssociatedTypeDecl: TypeDecl {

  /// The source range of the declaration's introducer, if any.
  public let introducerRange: SourceRange?

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The conformances listed in the declaration.
  public let conformances: [NodeID<NameExpr>]

  /// The where clause of the declaration, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public let defaultValue: AnyTypeExprID?

  /// Creates an instance with the given properties.
  public init(
    introducerRange: SourceRange?,
    identifier: SourceRepresentable<Identifier>,
    conformances: [NodeID<NameExpr>],
    whereClause: SourceRepresentable<WhereClause>?,
    defaultValue: AnyTypeExprID?
  ) {
    self.introducerRange = introducerRange
    self.identifier = identifier
    self.conformances = conformances
    self.whereClause = whereClause
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
