/// A subscript call.
public struct SubscriptCallExpr: Expr {

  public let origin: SourceRange?

  /// The callee.
  public let callee: AnyExprID

  /// The arguments of the call.
  public let arguments: [CallArgument]

  public init(callee: AnyExprID, arguments: [CallArgument], origin: SourceRange?) {
    self.origin = origin
    self.callee = callee
    self.arguments = arguments
  }

}
