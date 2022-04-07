/// A subscript call.
public struct SubscriptCallExpr: Expr {

  public var range: SourceRange?

  /// The callee.
  public var callee: Expr

  /// The arguments of the call.
  public var arguments: [Argument]

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(subscriptCall: self)
  }

}
