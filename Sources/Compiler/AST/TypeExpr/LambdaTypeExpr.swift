/// A lambda type expression.
public struct LambdaTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The environment of the lambda, or `nil` if it is thin.
  public var environment: TypeExpr?

  /// The parameters of the lambda.
  public var parameters: [ParamTypeExpr]

  /// The output type of the lambda.
  public var output: TypeExpr

}
