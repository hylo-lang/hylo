/// A parameter in a lambda type expression.
public struct ParameterTypeExpr: Expr {

  public let site: SourceRange

  /// The passing convention of the parameter.
  public let convention: SourceRepresentable<AccessEffect>

  /// The attributes of the declaration.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The expression of the parameter's bare type.
  public let bareType: AnyExprID

  public init(
    convention: SourceRepresentable<AccessEffect>,
    attributes: [SourceRepresentable<Attribute>],
    bareType: AnyExprID,
    site: SourceRange
  ) {
    self.site = site
    self.convention = convention
    self.attributes = attributes
    self.bareType = bareType
  }

}
