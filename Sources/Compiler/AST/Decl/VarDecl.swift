/// A variable declaration associated with a name in a binding declaration.
public struct VarDecl: SingleEntityDecl {

  /// The identifier of the declared variable.
  public var identifier: SourceRepresentable<Identifier>

  public var name: String { identifier.value }

}
