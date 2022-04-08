/// A yield statement.
public struct YieldStmt: Hashable {

  /// The yielded value.
  public var value: SourceRepresentable<Expr>

}
