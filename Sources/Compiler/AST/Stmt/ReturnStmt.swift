/// A return statement.
public struct ReturnStmt: Stmt {

  public static let kind = NodeKind.returnStmt

  /// The return value, if any.
  public var value: AnyExprIndex?

}
