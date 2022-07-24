/// A function call.
public struct FunCallExpr: Expr {

  public static let kind = NodeKind.funCallExpr

  /// The callee.
  public var callee: AnyExprID

  /// The arguments of the call.
  public var arguments: [CallArgument]

  public init(callee: AnyExprID, arguments: [CallArgument] = []) {
    self.callee = callee
    self.arguments = arguments
  }

}
