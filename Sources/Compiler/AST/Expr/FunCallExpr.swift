/// A function call.
public struct FunCallExpr: Hashable {

  /// A flag indicating whether the call is self-assigning.
  public var isSelfAssigning: Bool

  /// The callee.
  public var callee: SourceRepresentable<Expr>

  /// The arguments of the call.
  public var arguments: [SourceRepresentable<Argument>]

}
