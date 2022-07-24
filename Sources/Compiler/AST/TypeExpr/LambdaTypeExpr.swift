/// A lambda type expression.
public struct LambdaTypeExpr: TypeExpr {

  public static let kind = NodeKind.lambdaTypeExpr

  /// A parameter in a lambda type expression.
  public struct Parameter: Hashable {

    /// The label of the parameter.
    public var label: SourceRepresentable<Identifier>?

    /// The type of the parameter.
    public let type: NodeID<ParameterTypeExpr>

    public init(label: SourceRepresentable<String>? = nil, type: NodeID<ParameterTypeExpr>) {
      self.label = label
      self.type = type
    }

  }

  /// The effect of the lambda's call operator.
  public var receiverEffect: SourceRepresentable<ReceiverEffect>?

  /// The environment of the lambda, or `nil` if it is thin.
  public var environment: SourceRepresentable<AnyTypeExprID>?

  /// The parameters of the lambda.
  public var parameters: [Parameter]

  /// The output type of the lambda.
  public var output: AnyTypeExprID

  public init(
    receiverEffect: SourceRepresentable<ReceiverEffect>? = nil,
    environment: SourceRepresentable<AnyTypeExprID>? = nil,
    parameters: [Parameter] = [],
    output: AnyTypeExprID
  ) {
    self.receiverEffect = receiverEffect
    self.environment = environment
    self.parameters = parameters
    self.output = output
  }

}
