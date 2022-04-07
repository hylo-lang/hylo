/// A declaration statement.
public struct DeclStmt: Stmt {

  public var range: SourceRange?

  /// The declaration.
  public var decl: AnyDeclIndex

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(decl: self)
  }

}
