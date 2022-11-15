/// A function call.
public struct FunCallExpr: Expr {

  /// The callee.
  public let callee: AnyExprID

  /// The arguments of the call.
  public let arguments: [CallArgument]

  public init(callee: AnyExprID, arguments: [CallArgument] = []) {
    self.callee = callee
    self.arguments = arguments
  }

}
