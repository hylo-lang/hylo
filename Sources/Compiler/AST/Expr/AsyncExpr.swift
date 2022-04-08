/// An expression ran in a future.
public struct AsyncExpr: Hashable {

  /// The declaration of the underlying anonymous function.
  public var decl: DeclIndex<FunDecl>

}
