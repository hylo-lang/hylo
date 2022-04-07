/// A nil expression.
public struct NilExpr: Expr {

  public var range: SourceRange?

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(nil: self)
  }

}
