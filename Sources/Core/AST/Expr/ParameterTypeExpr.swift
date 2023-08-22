/// A parameter in a lambda type expression.
public struct ParameterTypeExpr: Expr {

  public let site: SourceRange

  /// The passing convention of the parameter.
  public let convention: SourceRepresentable<AccessEffect>

  /// The expression of the parameter's bare type.
  public let bareType: AnyExprID

  public init(
    convention: SourceRepresentable<AccessEffect>,
    bareType: AnyExprID,
    site: SourceRange
  ) {
    self.site = site
    self.convention = convention
    self.bareType = bareType
  }

}
