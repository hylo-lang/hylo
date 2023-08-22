/// A variable declaration associated with a name in a binding declaration.
public struct VarDecl: SingleEntityDecl {

  public static let constructDescription = "variable declaration"

  /// The identifier of the declared variable.
  public let identifier: SourceRepresentable<Identifier>

  public init(identifier: SourceRepresentable<Identifier>) {
    self.identifier = identifier
  }

  public var site: SourceRange { identifier.site }

  public var baseName: String { identifier.value }

}
