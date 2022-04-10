/// A parameter in a lambda type expression.
public struct ParamTypeExpr: TypeExpr {

  /// The label of the parameter.
  public var label: SourceRepresentable<Identifier>?

  /// The passing convention of the parameter.
  public var convention: SourceRepresentable<ParamConvention>

  /// The expression of the parameter's bare type.
  public var bareType: AnyTypeExprIndex

}
