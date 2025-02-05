/// A subscript call.
public struct SubscriptCallExpr: Expr, Sendable {

  public let site: SourceRange

  /// The callee.
  public let callee: AnyExprID

  /// The arguments of the call.
  public let arguments: [LabeledArgument]

  public init(callee: AnyExprID, arguments: [LabeledArgument], site: SourceRange) {
    self.site = site
    self.callee = callee
    self.arguments = arguments
  }

}
