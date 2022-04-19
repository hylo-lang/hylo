/// A yield statement.
public struct YieldStmt: Stmt {

  public static let kind = NodeKind.yieldStmt

  /// The yielded value.
  public var value: AnyExprID

  public init(value: AnyExprID) {
    self.value = value
  }

}
