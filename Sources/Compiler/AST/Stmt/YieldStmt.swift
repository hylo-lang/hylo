/// A yield statement.
public struct YieldStmt: Stmt {

  public var range: SourceRange?

  /// The yielded value.
  public var value: Expr

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(yield: self)
  }

}

extension YieldStmt: CustomStringConvertible {

  public var description: String { "yield \(value)" }

}
