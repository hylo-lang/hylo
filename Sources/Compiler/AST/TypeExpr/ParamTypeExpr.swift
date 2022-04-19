/// A parameter in a lambda type expression.
public struct ParamTypeExpr: TypeExpr {

  public static let kind = NodeKind.paramTypeExpr

  /// The label of the parameter.
  public var label: SourceRepresentable<Identifier>?

  /// The passing convention of the parameter.
  public var convention: SourceRepresentable<ParamConvention>

  /// The expression of the parameter's bare type.
  public var bareType: AnyTypeExprID

  public init(
    label: SourceRepresentable<Identifier>? = nil,
    convention: SourceRepresentable<ParamConvention>,
    bareType: AnyTypeExprID
  ) {
    self.label = label
    self.convention = convention
    self.bareType = bareType
  }

}
