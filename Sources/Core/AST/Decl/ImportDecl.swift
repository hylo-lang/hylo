/// An import declaration.
public struct ImportDecl: SingleEntityDecl {

  public static let constructDescription = "import declaration"

  public let site: SourceRange

  /// The site of the declaration's introducer.
  public let introducerSite: SourceRange

  /// The identifier of the imported module.
  public let identifier: SourceRepresentable<Identifier>

  /// Creates an instance with the given properties.
  public init(
    introducerSite: SourceRange,
    identifier: SourceRepresentable<Identifier>,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.identifier = identifier
  }

  public var baseName: String { identifier.value }

}
