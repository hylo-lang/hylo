/// A yield statement.
public struct YieldStmt: Stmt {

  /// The yielded value.
  public let value: AnyExprID

  public init(value: AnyExprID) {
    self.value = value
  }

}
