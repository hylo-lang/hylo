/// A buffer literal expression.
public struct BufferLiteralExpr: Expr {

  public static let kind = NodeKind.bufferLiteralExpr

  /// The elements of the literal.
  public var elements: [AnyExprID]

  public init(elements: [AnyExprID]) {
    self.elements = elements
  }

}
