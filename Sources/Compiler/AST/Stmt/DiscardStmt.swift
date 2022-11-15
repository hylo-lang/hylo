/// A discard statement.
public struct DiscardStmt: Stmt {

  /// The expression whose value is discarded.
  public let expr: AnyExprID

  public init(expr: AnyExprID) {
    self.expr = expr
  }

}
