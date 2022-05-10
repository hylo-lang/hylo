/// A lambda type expression.
public struct LambdaTypeExpr: TypeExpr {

  public static let kind = NodeKind.lambdaTypeExpr

  /// A parameter in a lambda type expression.
  public struct Parameter: Hashable {

    /// The label of the parameter.
    public let label: String?

    /// The type of the parameter.
    public let type: NodeID<ParameterTypeExpr>

  }

  /// The environment of the lambda, or `nil` if it is thin.
  public var environment: AnyTypeExprID?

  /// The parameters of the lambda.
  public var parameters: [Parameter]

  /// The output type of the lambda.
  public var output: AnyTypeExprID

  public init(
    environment: AnyTypeExprID? = nil,
    parameters: [Parameter] = [],
    output: AnyTypeExprID
  ) {
    self.environment = environment
    self.parameters = parameters
    self.output = output
  }

}
