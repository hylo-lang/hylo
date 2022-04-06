/// A parameter in a lambda type expression.
public struct ParamTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The label of the parameter.
  public var label: Identifier?

  /// The passing convention of the parameter.
  public var convention: ParamConvention

  /// The expression of the parameter's bare type.
  public var bareType: TypeExpr

}
