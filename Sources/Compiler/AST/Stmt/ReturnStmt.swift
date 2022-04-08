/// A return statement.
public struct ReturnStmt: Hashable {

  /// The return value, if any.
  public var value: SourceRepresentable<Expr>?

}
