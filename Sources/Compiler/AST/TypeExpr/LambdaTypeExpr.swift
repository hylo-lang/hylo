/// A lambda type expression.
public struct LambdaTypeExpr: Hashable {

  /// The environment of the lambda, or `nil` if it is thin.
  public var environment: SourceRepresentable<TypeExpr>?

  /// The parameters of the lambda.
  public var parameters: [SourceRepresentable<ParamTypeExpr>]

  /// The output type of the lambda.
  public var output: SourceRepresentable<TypeExpr>

}
