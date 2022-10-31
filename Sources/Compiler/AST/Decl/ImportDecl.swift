/// An import declaration.
public struct ImportDecl: SingleEntityDecl {

  public static let kind = NodeKind.importDecl

  /// The identifier of the imported module.
  public let identifier: SourceRepresentable<Identifier>

  public init(identifier: SourceRepresentable<Identifier>) {
    self.identifier = identifier
  }

  public var name: String { identifier.value }

}
