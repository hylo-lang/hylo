/// A buffer literal expression.
public struct BufferLiteralExpr: Expr {

  public let site: SourceRange

  /// The elements of the literal.
  public let elements: [AnyExprID]

  public init(elements: [AnyExprID], site: SourceRange) {
    self.site = site
    self.elements = elements
  }

}
