/// A return statement.
public struct ReturnStmt: Stmt {

  public let origin: SourceRange?

  /// The return value, if any.
  public let value: AnyExprID?

  public init(value: AnyExprID?, origin: SourceRange?) {
    self.origin = origin
    self.value = value
  }

}
