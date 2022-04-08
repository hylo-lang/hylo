/// A buffer literal expression.
public struct BufferLiteralExpr: Hashable {

  /// The elements of the literal.
  public var elements: [SourceRepresentable<Expr>]

}
