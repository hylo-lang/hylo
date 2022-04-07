/// A break statement.
public struct BreakStmt: Stmt {

  public var range: SourceRange?

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(break: self)
  }

}
