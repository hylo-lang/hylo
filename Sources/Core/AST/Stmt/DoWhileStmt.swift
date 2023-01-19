/// A do-while loop.
public struct DoWhileStmt: Stmt {

  public let origin: SourceRange

  /// The body of the loop.
  public let body: NodeID<BraceStmt>

  /// The condition of the loop.
  ///
  /// - Note: The condition is evaluated in the lexical scope of the body.
  public let condition: AnyExprID

  public init(body: NodeID<BraceStmt>, condition: AnyExprID, origin: SourceRange) {
    self.origin = origin
    self.body = body
    self.condition = condition
  }

}
