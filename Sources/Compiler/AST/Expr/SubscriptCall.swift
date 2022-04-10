/// A subscript call.
public struct SubscriptCallExpr: Expr {

  /// The callee.
  public var callee: AnyExprIndex

  /// The arguments of the call.
  public var arguments: [SourceRepresentable<Argument>]

}
