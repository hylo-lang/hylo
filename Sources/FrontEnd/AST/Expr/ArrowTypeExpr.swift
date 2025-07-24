/// An arrow type expression.
public struct ArrowTypeExpr: Expr, Sendable {

  /// A parameter in an arrow type expression.
  public struct Parameter: Codable, Sendable {

    /// The label of the parameter.
    public var label: SourceRepresentable<Identifier>?

    /// The type of the parameter.
    public let type: ParameterTypeExpr.ID

    public init(label: SourceRepresentable<String>? = nil, type: ParameterTypeExpr.ID) {
      self.label = label
      self.type = type
    }

  }

  public private(set) var site: SourceRange

  /// The effect of the arrow's call operator.
  public let receiverEffect: SourceRepresentable<AccessEffect>?

  /// The environment of the arrow, or `nil` if it is thin.
  public private(set) var environment: AnyExprID?

  /// The parameters of the arrow.
  public let parameters: [Parameter]

  /// The output type of the arrow.
  public let output: AnyExprID

  public init(
    receiverEffect: SourceRepresentable<AccessEffect>?,
    environment: AnyExprID?,
    parameters: [Parameter],
    output: AnyExprID,
    site: SourceRange
  ) {
    self.site = site
    self.receiverEffect = receiverEffect
    self.environment = environment
    self.parameters = parameters
    self.output = output
  }

}
