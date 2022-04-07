/// A return statement.
public struct ReturnStmt: Stmt {

  public var range: SourceRange?

  /// The return value, if any.
  public var value: Expr?

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(return: self)
  }

}

extension ReturnStmt: CustomStringConvertible {

  public var description: String {
    if let value = value {
      return "return \(value)"
    } else {
      return "return"
    }
  }

}
