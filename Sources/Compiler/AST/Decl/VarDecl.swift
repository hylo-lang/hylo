/// A variable declaration associated with a name in a binding declaration.
public struct VarDecl: SingleEntityDecl {

  /// The identifier of the declared variable.
  public let identifier: SourceRepresentable<Identifier>

  public init(identifier: SourceRepresentable<Identifier>) {
    self.identifier = identifier
  }

  public var origin: SourceRange? { identifier.range }

  public var name: String { identifier.value }

}
