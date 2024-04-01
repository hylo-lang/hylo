/// A parameter in an arrow type expression.
public struct ParameterTypeExpr: Expr {

  public let site: SourceRange

  /// The passing convention of the parameter.
  public let convention: SourceRepresentable<AccessEffect>

  /// The expression of the parameter's bare type.
  public let bareType: AnyExprID

  /// `true` if arguments to the parameter are automatically wrapped in lambdas.
  public let isAutoclosure: Bool

  public init(
    convention: SourceRepresentable<AccessEffect>,
    isAutoclosure: Bool,
    bareType: AnyExprID,
    site: SourceRange
  ) {
    self.site = site
    self.convention = convention
    self.isAutoclosure = isAutoclosure
    self.bareType = bareType
  }

}
