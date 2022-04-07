/// A while loop.
public struct WhileStmt: Stmt, ScopeOutliner {

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The condition of the loop.
  public var condition: [ConditionItem]

  /// The body of the loop.
  public var body: BraceStmt

}

extension WhileStmt: CustomStringConvertible {

  public var description: String {
    "while \(String.joining(condition, separator: ", ")) \(body)"
  }

}
