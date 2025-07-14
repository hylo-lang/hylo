/// An assignment statement.
public struct AssignStmt: Stmt, Sendable {

  public let site: SourceRange

  /// The left operand.
  public let left: AnyExprID

  /// The right operand.
  public let right: AnyExprID

  public init(left: AnyExprID, right: AnyExprID, site: SourceRange) {
    self.site = site
    self.left = left
    self.right = right
  }

}
