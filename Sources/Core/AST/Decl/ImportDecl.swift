/// An import declaration.
public struct ImportDecl: SingleEntityDecl {

  public let site: SourceRange

  /// The source range of the declaration's introducer.
  public let introducerRange: SourceRange

  /// The identifier of the imported module.
  public let identifier: SourceRepresentable<Identifier>

  /// Creates an instance with the given properties.
  public init(
    introducerRange: SourceRange,
    identifier: SourceRepresentable<Identifier>,
    site: SourceRange
  ) {
    self.site = site
    self.introducerRange = introducerRange
    self.identifier = identifier
  }

  public var name: String { identifier.value }

}
