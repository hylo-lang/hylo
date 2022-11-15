/// An import declaration.
public struct ImportDecl: TypeDecl {

  /// The source range of the declaration's introducer, if any.
  public let introducerRange: SourceRange?

  /// The identifier of the imported module.
  public let identifier: SourceRepresentable<Identifier>

  /// Creates an instance with the given properties.
  public init(introducerRange: SourceRange?, identifier: SourceRepresentable<Identifier>) {
    self.introducerRange = introducerRange
    self.identifier = identifier
  }

  public var name: String { identifier.value }

}
