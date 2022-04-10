/// A lambda type expression.
public struct LambdaTypeExpr: TypeExpr {

  /// The environment of the lambda, or `nil` if it is thin.
  public var environment: AnyTypeExprIndex?

  /// The parameters of the lambda.
  public var parameters: [NodeIndex<ParamTypeExpr>]

  /// The output type of the lambda.
  public var output: AnyTypeExprIndex

}
