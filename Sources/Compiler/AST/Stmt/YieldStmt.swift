/// A yield statement.
public struct YieldStmt: Stmt {

  public var range: SourceRange?

  /// The yielded value.
  public var value: Expr

}

extension YieldStmt: CustomStringConvertible {

  public var description: String { "yield \(value)" }

}
