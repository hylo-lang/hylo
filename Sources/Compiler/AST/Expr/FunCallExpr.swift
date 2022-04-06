/// A function call.
public struct FunCallExpr: Expr {

  public var range: SourceRange?

  /// A flag indicating whether the call is self-assigning.
  public var isSelfAssigning: Bool

  /// The callee.
  public var callee: Expr

  /// The arguments of the call.
  public var arguments: [Argument]

}
