/// A parameter declaration in a function or subscript declaration.
public struct ParameterDecl: SingleEntityDecl {

  public static let constructDescription = "parameter declaration"

  public let site: SourceRange

  /// The label of the parameter.
  public let label: SourceRepresentable<Identifier>?

  /// The identifier of the parameter.
  public let identifier: SourceRepresentable<Identifier>

  /// The type annotation of the declaration, if any.
  public let annotation: ParameterTypeExpr.ID?

  /// The default value of the declaration, if any.
  public let defaultValue: AnyExprID?

  public init(
    label: SourceRepresentable<Identifier>? = nil,
    identifier: SourceRepresentable<Identifier>,
    annotation: ParameterTypeExpr.ID? = nil,
    defaultValue: AnyExprID? = nil,
    site: SourceRange
  ) {
    self.site = site
    self.label = label
    self.identifier = identifier
    self.annotation = annotation
    self.defaultValue = defaultValue
  }

  public var baseName: String { identifier.value }

}
