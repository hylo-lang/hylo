/// A buffer literal expression.
public struct BufferLiteralExpr: Expr {

  public let origin: SourceRange

  /// The elements of the literal.
  public let elements: [AnyExprID]

  public init(elements: [AnyExprID], origin: SourceRange) {
    self.origin = origin
    self.elements = elements
  }

}
