/// The identity of a call expression.
public enum CallID: Hashable, Sendable {

  case ast(AnyExprID)

  case infix(FoldedSequenceExpr)

  public init(_ e: FunctionCallExpr.ID) {
    self = .ast(AnyExprID(e))
  }

  public init(_ e: SubscriptCallExpr.ID) {
    self = .ast(AnyExprID(e))
  }

  public init(_ e: FoldedSequenceExpr) {
    self = .infix(e)
  }

}
