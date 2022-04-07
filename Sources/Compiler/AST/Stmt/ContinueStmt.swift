/// A continue statement.
public struct ContinueStmt: Stmt {

  public var range: SourceRange?

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(continue: self)
  }

}
