/// The expression of an existential type.
public struct ExistentialTypeExpr: Expr {

  public let site: SourceRange

  /// The site of the `any` keyword.
  public let introducerSite: SourceRange

  /// The traits to which the witness conforms.
  public let traits: TraitComposition

  /// The where clause of the expression, if any.
  public let whereClause: SourceRepresentable<WhereClause>?

  public init(
    introducerSite: SourceRange,
    traits: TraitComposition,
    whereClause: SourceRepresentable<WhereClause>?,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.traits = traits
    self.whereClause = whereClause
  }

}
