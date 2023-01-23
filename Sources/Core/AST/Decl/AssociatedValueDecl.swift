/// An associated value declaration.
public struct AssociatedValueDecl: SingleEntityDecl {

  public let origin: SourceRange

  /// The source range of the declaration's introducer.
  public let introducerRange: SourceRange

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The where clause of the declaration, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  /// Creates an instance with the given properties.
  public init(
    introducerRange: SourceRange,
    identifier: SourceRepresentable<Identifier>,
    whereClause: SourceRepresentable<WhereClause>?,
    defaultValue: AnyExprID?,
    origin: SourceRange
  ) {
    self.origin = origin
    self.introducerRange = introducerRange
    self.identifier = identifier
    self.whereClause = whereClause
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
