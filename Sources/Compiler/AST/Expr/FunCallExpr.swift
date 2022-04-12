/// A function call.
public struct FunCallExpr: Expr {

  public static let kind = NodeKind.funCallExpr

  /// A flag indicating whether the call is self-assigning.
  public var isSelfAssigning: Bool

  /// The callee.
  public var callee: AnyExprID

  /// The arguments of the call.
  public var arguments: [SourceRepresentable<Argument>]

}
