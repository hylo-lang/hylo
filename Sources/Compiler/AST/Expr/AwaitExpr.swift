/// An await expression.
public struct AwaitExpr: Expr {

  public var range: SourceRange?

  /// The expression of the awaited value.
  public var operand: Expr

}
