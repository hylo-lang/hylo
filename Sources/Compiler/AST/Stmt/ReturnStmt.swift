/// A return statement.
public struct ReturnStmt: Stmt {

  public var range: SourceRange?

  /// The return value, if any.
  public var value: Expr?

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
