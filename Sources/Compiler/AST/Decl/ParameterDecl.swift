/// A parameter declaration in a function or subscript declaration.
public struct ParameterDecl: SingleEntityDecl {

  public static let kind = NodeKind.parameterDecl

  /// The label of the parameter.
  public var label: SourceRepresentable<Identifier>?

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The type annotation of the declaration, if any.
  public var annotation: NodeID<ParameterTypeExpr>?

  /// The default value of the declaration, if any.
  public var defaultValue: AnyExprID?

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
