/// A variable declaration associated with a name in a binding declaration.
public struct VarDecl: SingleEntityDecl {

  public static let kind = NodeKind.varDecl

  /// The identifier of the declared variable.
  public var identifier: SourceRepresentable<Identifier>

  public init(identifier: SourceRepresentable<Identifier>) {
    self.identifier = identifier
  }

  public var name: String { identifier.value }

}
