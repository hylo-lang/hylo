/// A lambda type expression.
public struct LambdaTypeExpr: TypeExpr {

  public static let kind = NodeKind.lambdaTypeExpr

  /// The environment of the lambda, or `nil` if it is thin.
  public var environment: AnyTypeExprID?

  /// The parameters of the lambda.
  public var parameters: [NodeID<ParamTypeExpr>]

  /// The output type of the lambda.
  public var output: AnyTypeExprID

}
