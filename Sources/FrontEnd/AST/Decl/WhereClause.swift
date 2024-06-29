import Utils

/// A where clause.
public struct WhereClause: Codable {

  /// The site of the `where` introducer.
  public let introducerSite: SourceRange

  /// The expression of a type constraint defined in a generic clause.
  public enum ConstraintExpr: Codable {

    /// An equality constraint involving one or two skolems.
    case equality(l: NameExpr.ID, r: AnyExprID)

    /// A conformance or instance constraint on a skolem.
    case bound(l: NameExpr.ID, r: [NameExpr.ID])

    /// A constraint on a value parameter.
    case value(AnyExprID)

  }

  /// The constraint expressions in the clause.
  public let constraints: [SourceRepresentable<ConstraintExpr>]

  public init(
    introducerSite: SourceRange,
    constraints: [SourceRepresentable<WhereClause.ConstraintExpr>]
  ) {
    self.introducerSite = introducerSite
    self.constraints = constraints
  }

}
