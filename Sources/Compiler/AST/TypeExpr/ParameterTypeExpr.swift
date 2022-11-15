/// A parameter in a lambda type expression.
public struct ParameterTypeExpr: TypeExpr {

  /// The passing convention of the parameter.
  public let convention: SourceRepresentable<PassingConvention>

  /// The expression of the parameter's bare type.
  public let bareType: AnyTypeExprID

  public init(
    convention: SourceRepresentable<PassingConvention>,
    bareType: AnyTypeExprID
  ) {
    self.convention = convention
    self.bareType = bareType
  }

}
