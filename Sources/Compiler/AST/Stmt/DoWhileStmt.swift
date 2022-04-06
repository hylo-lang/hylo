/// A do-while loop.
public struct DoWhileStmt: Stmt {

  public var range: SourceRange?

  /// The body of the loop.
  public var body: BraceStmt

  /// The condition of the loop.
  public var condition: Expr

}

extension DoWhileStmt: CustomStringConvertible {

  public var description: String {
    "do \(body) while \(condition)"
  }

}
