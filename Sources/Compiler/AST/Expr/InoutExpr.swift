/// An expression evaluated in place.
public struct InoutExpr: Expr {

  /// The source range of the `&` operator.
  public let operatorRange: SourceRange?

  /// The underlying expression.
  public let subject: AnyExprID

  public init(operatorRange: SourceRange? = nil, subject: AnyExprID) {
    self.operatorRange = operatorRange
    self.subject = subject
  }

}
