/// A parameter declaration in a function or subscript declaration.
public struct ParameterDecl: SingleEntityDecl {

  /// The label of the parameter.
  public let label: SourceRepresentable<Identifier>?

  /// The identifier of the parameter.
  public let identifier: SourceRepresentable<Identifier>

  /// The type annotation of the declaration, if any.
  public let annotation: NodeID<ParameterTypeExpr>?

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  public init(
    label: SourceRepresentable<Identifier>? = nil,
    identifier: SourceRepresentable<Identifier>,
    annotation: NodeID<ParameterTypeExpr>? = nil,
    defaultValue: AnyExprID? = nil
  ) {
    self.label = label
    self.identifier = identifier
    self.annotation = annotation
    self.defaultValue = defaultValue
  }

  public var name: String { identifier.value }

}
