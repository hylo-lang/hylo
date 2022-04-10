/// An expression ran in a future.
public struct AsyncExpr: Expr {

  /// The declaration of the underlying anonymous function.
  public var decl: NodeIndex<FunDecl>

}
