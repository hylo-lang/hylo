/// A lambda type expression.
public struct LambdaTypeExpr: TypeExpr {

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

  /// The effect of the lambda's call operator.
  public let receiverEffect: SourceRepresentable<ReceiverEffect>?

  /// The environment of the lambda, or `nil` if it is thin.
  public private(set) var environment: SourceRepresentable<AnyTypeExprID>?

  /// The parameters of the lambda.
  public let parameters: [Parameter]

  /// The output type of the lambda.
  public let output: AnyTypeExprID

  public init(
    receiverEffect: SourceRepresentable<ReceiverEffect>? = nil,
    parameters: [Parameter] = [],
    output: AnyTypeExprID
  ) {
    self.receiverEffect = receiverEffect
    self.parameters = parameters
    self.output = output
  }

  /// Incorporates `accessModifier` into `self`.
  ///
  /// - Precondition: `self.environment == nil`
  internal mutating func incorporate(environment: SourceRepresentable<AnyTypeExprID>?) {
    precondition(self.environment == nil)
    self.environment = environment
  }
}
