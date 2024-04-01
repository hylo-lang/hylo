/// A declaration that extends a type with new conformances.
public struct ConformanceDecl: ConformanceSource, TypeExtendingDecl {

  public static let constructDescription = "conformance declaration"

  public let site: SourceRange

  /// The site of the `conformance` introducer.
  public let introducerSite: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The expression of the extended type.
  public let subject: AnyExprID

  /// The names of traits to which conformance is declared.
  public let conformances: [NameExpr.ID]

  /// The condition of the conformance, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  /// The member declarations in the lexical scope of the conformance.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    accessModifier: SourceRepresentable<AccessModifier>,
    subject: AnyExprID,
    conformances: [NameExpr.ID],
    whereClause: SourceRepresentable<WhereClause>?,
    members: [AnyDeclID],
    site: SourceRange
  ) {
    self.introducerSite = introducerSite
    self.site = site
    self.accessModifier = accessModifier
    self.subject = subject
    self.conformances = conformances
    self.whereClause = whereClause
    self.members = members
  }

}
