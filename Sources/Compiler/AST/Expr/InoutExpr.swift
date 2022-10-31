/// An expression evaluated in place.
public struct InoutExpr: Expr {

  public static let kind = NodeKind.inoutExpr

  /// The source range of the `&` operator.
  public let operatorRange: SourceRange?

  /// The underlying expression.
  public let subexpr: AnyExprID

  public init(operatorRange: SourceRange? = nil, subexpr: AnyExprID) {
    self.operatorRange = operatorRange
    self.subexpr = subexpr
  }

}
