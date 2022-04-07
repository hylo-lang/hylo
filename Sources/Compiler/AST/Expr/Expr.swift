/// A value expression.
public protocol Expr: SourceRepresentable {

  /// Accepts the specified visitor.
  func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result

}
