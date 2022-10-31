/// A return statement.
public struct ReturnStmt: Stmt {

  public static let kind = NodeKind.returnStmt

  /// The return value, if any.
  public let value: AnyExprID?

  public init(value: AnyExprID? = nil) {
    self.value = value
  }

}
