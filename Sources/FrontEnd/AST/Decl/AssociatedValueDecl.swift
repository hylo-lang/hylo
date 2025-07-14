/// An associated value declaration.
public struct AssociatedValueDecl: SingleEntityDecl, Sendable {

  public static let constructDescription = "associated value declaration"

  public let site: SourceRange

  /// The site of the declaration's introducer.
  public let introducerSite: SourceRange

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The where clause of the declaration, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    identifier: SourceRepresentable<Identifier>,
    whereClause: SourceRepresentable<WhereClause>?,
    defaultValue: AnyExprID?,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.identifier = identifier
    self.whereClause = whereClause
    self.defaultValue = defaultValue
  }

  public var baseName: String { identifier.value }

}
