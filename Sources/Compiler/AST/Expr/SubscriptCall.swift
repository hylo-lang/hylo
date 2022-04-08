/// A subscript call.
public struct SubscriptCallExpr: Hashable {

  /// The callee.
  public var callee: SourceRepresentable<Expr>

  /// The arguments of the call.
  public var arguments: [SourceRepresentable<Argument>]

}
