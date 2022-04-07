/// An expression ran in a future.
public struct AsyncExpr: Expr {

  public var range: SourceRange?

  /// The declaration of the underlying anonymous function.
  public var decl: DeclIndex<FunDecl>

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(async: self)
  }

}
