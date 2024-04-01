/// An operator declaration.
public struct OperatorDecl: ExposableDecl {

  public static let constructDescription = "operator declaration"

  public let site: SourceRange

  /// The site of the declaration's introducer.
  public let introducerSite: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>

  /// The notation of the operator.
  public let notation: SourceRepresentable<OperatorNotation>

  /// The name of the operator.
  public let name: SourceRepresentable<Identifier>

  /// The precedence group of the operator, if any.
  public let precedenceGroup: SourceRepresentable<PrecedenceGroup>?

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    accessModifier: SourceRepresentable<AccessModifier>,
    notation: SourceRepresentable<OperatorNotation>,
    name: SourceRepresentable<Identifier>,
    precedenceGroup: SourceRepresentable<PrecedenceGroup>?,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.accessModifier = accessModifier
    self.notation = notation
    self.name = name
    self.precedenceGroup = precedenceGroup
  }

}
