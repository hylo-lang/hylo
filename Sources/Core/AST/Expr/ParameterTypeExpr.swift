/// A parameter in a lambda type expression.
public struct ParameterTypeExpr: Expr {

  public let origin: SourceRange

  /// The passing convention of the parameter.
  public let convention: SourceRepresentable<AccessEffect>

  /// The expression of the parameter's bare type.
  public let bareType: AnyTypeExprID

  /// Indicates whether `convention` was synthesized.
  public let isConventionSynthesized: Bool

  public init(
    convention: SourceRepresentable<AccessEffect>,
    bareType: AnyTypeExprID,
    origin: SourceRange,
    synthesized: Bool
  ) {
    self.origin = origin
    self.convention = convention
    self.bareType = bareType
    self.isConventionSynthesized = synthesized
  }

}
