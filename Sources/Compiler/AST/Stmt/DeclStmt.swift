/// A declaration statement.
public struct DeclStmt: Stmt {

  public var range: SourceRange?

  /// The declaration.
  public var decl: AnyDeclIndex

}
