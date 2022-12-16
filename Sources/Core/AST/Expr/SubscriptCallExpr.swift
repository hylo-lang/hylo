/// A subscript call.
public struct SubscriptCallExpr: Expr {

  public let origin: SourceRange?

  /// The callee.
  public let callee: AnyExprID

  /// The arguments of the call.
  public let arguments: [LabeledArgument]

  public init(callee: AnyExprID, arguments: [LabeledArgument], origin: SourceRange?) {
    self.origin = origin
    self.callee = callee
    self.arguments = arguments
  }

}
