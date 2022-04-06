/// A buffer literal expression.
public struct BufferLiteralExpr: Expr {

  public var range: SourceRange?

  /// The elements of the literal.
  public var elements: [Expr]

}
