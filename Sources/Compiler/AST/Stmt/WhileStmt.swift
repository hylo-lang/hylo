/// A while loop.
public struct WhileStmt: Stmt, ScopeOutliner {

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The condition of the loop.
  public var condition: [ConditionItem]

  /// The body of the loop.
  public var body: BraceStmt

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(while: self)
  }

}

extension WhileStmt: CustomStringConvertible {

  public var description: String {
    "while \(String.joining(condition, separator: ", ")) \(body)"
  }

}
