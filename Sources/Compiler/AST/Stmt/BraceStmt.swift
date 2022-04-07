/// A brace statement (a.k.a. a code block).
public struct BraceStmt: Stmt, ScopeOutliner {

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The statements in the block.
  public var stmts: [Stmt]

}

extension BraceStmt: CustomStringConvertible {

  public var description: String {
    var result = "{"
    for stmt in stmts {
      let lines = String(describing: stmt).split(separator: "\n")
      for line in lines {
        result += "  \(line)"
      }
    }
    result += "}"
    return result
  }

}
