/// A lambda.
public struct LambdaExpr: Expr {

  public var range: SourceRange?

  /// The declaration of the underlying anonymous function.
  public var decl: DeclIndex<FunDecl>

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(lambda: self)
  }

}
