/// A yield statement.
public struct YieldStmt: Stmt {

  public let origin: SourceRange

  /// The yielded value.
  public let value: AnyExprID

  public init(value: AnyExprID, origin: SourceRange) {
    self.origin = origin
    self.value = value
  }

}
