/// A lambda type expression.
public struct LambdaTypeExpr: Expr {

  /// A parameter in a lambda type expression.
  public struct Parameter: Codable {

    /// The label of the parameter.
    public var label: SourceRepresentable<Identifier>?

    /// The type of the parameter.
    public let type: NodeID<ParameterTypeExpr>

    public init(label: SourceRepresentable<String>? = nil, type: NodeID<ParameterTypeExpr>) {
      self.label = label
      self.type = type
    }

  }

  public private(set) var origin: SourceRange

  /// The effect of the lambda's call operator.
  public let receiverEffect: SourceRepresentable<AccessEffect>?

  /// The environment of the lambda, or `nil` if it is thin.
  public private(set) var environment: AnyTypeExprID?

  /// The parameters of the lambda.
  public let parameters: [Parameter]

  /// The output type of the lambda.
  public let output: AnyExprID

  public init(
    receiverEffect: SourceRepresentable<AccessEffect>?,
    environment: AnyExprID?,
    parameters: [Parameter],
    output: AnyTypeExprID,
    origin: SourceRange
  ) {
    self.origin = origin
    self.receiverEffect = receiverEffect
    self.environment = environment
    self.parameters = parameters
    self.output = output
  }

}
