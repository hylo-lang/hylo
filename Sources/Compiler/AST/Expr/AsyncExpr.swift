/// An expression ran in a future.
public struct AsyncExpr: Expr {

  public var range: SourceRange?

  /// The declaration of the underlying anonymous function.
  public var decl: DeclIndex<FunDecl>

}
