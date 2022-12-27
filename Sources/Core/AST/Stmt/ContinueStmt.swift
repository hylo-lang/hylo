/// A continue statement.
public struct ContinueStmt: Stmt {

  public let origin: SourceRange?

  public init(origin: SourceRange?) {
    self.origin = origin
  }

}
