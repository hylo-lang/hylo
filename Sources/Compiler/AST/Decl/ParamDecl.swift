/// A parameter declaration in a function or subscript declaration.
public struct ParamDecl: SingleEntityDecl {

  public static let kind = NodeKind.paramDecl

  /// The label of the parameter.
  public var label: SourceRepresentable<Identifier>?

  /// The identifier of the parameter.
  public var identifier: SourceRepresentable<Identifier>

  /// The type annotation of the declaration, if any.
  public var annotation: AnyTypeExprID?

  /// The default value of the declaration, if any.
  public var defaultValue: AnyExprID?

  public var name: String { identifier.value }

  public init(
    label: SourceRepresentable<Identifier>? = nil,
    identifier: SourceRepresentable<Identifier>,
    annotation: AnyTypeExprID? = nil,
    defaultValue: AnyExprID? = nil
  ) {
    self.label = label
    self.identifier = identifier
    self.annotation = annotation
    self.defaultValue = defaultValue
  }

}
