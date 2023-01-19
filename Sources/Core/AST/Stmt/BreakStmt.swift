/// A break statement.
public struct BreakStmt: Stmt {

  public let origin: SourceRange

  public init(origin: SourceRange) {
    self.origin = origin
  }

}
