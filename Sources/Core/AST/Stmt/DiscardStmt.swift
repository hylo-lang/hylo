/// A discard statement.
public struct DiscardStmt: Stmt {

  public let origin: SourceRange

  /// The expression whose value is discarded.
  public let expr: AnyExprID

  public init(expr: AnyExprID, origin: SourceRange) {
    self.origin = origin
    self.expr = expr
  }

}
