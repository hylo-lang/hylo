/// A do-while loop.
public struct DoWhileStmt: Hashable {

  /// The body of the loop.
  public var body: SourceRepresentable<BraceStmt>

  /// The condition of the loop.
  ///
  /// - Note: The condition is evaluated in the lexical scope of the body.
  public var condition: SourceRepresentable<Expr>

}
