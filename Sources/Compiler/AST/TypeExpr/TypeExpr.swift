/// A type expression.
public protocol TypeExpr: SourceRepresentable {

  /// Accepts the specified visitor.
  func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result

}
