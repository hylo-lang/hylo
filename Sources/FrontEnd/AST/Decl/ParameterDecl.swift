/// A parameter declaration in a function or subscript declaration.
public struct ParameterDecl: SingleEntityDecl, Sendable {

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

  /// `true` if arguments to the parameter can be passed implicitly.
  public let isImplicit: Bool

  public init(
    label: SourceRepresentable<Identifier>? = nil,
    identifier: SourceRepresentable<Identifier>,
    annotation: ParameterTypeExpr.ID? = nil,
    defaultValue: AnyExprID? = nil,
    isImplicit: Bool = false,
    site: SourceRange
  ) {
    self.site = site
    self.label = label
    self.identifier = identifier
    self.annotation = annotation
    self.defaultValue = defaultValue
    self.isImplicit = isImplicit
  }

  public var baseName: String { identifier.value }

}
