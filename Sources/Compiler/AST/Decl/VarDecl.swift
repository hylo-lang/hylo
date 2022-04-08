/// A variable declaration associated with a name in a binding declaration.
public struct VarDecl: Decl {

  /// The identifier of the declared variable.
  public var identifier: SourceRepresentable<Identifier>

  public var range: SourceRange?

}
