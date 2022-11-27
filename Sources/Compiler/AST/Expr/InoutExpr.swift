/// An expression evaluated in place.
public struct InoutExpr: Expr {

  public let origin: SourceRange?

  /// The source range of the `&` operator.
  public let operatorRange: SourceRange?

  /// The underlying expression.
  public let subject: AnyExprID

  public init(operatorRange: SourceRange?, subject: AnyExprID, origin: SourceRange?) {
    self.origin = origin
    self.operatorRange = operatorRange
    self.subject = subject
  }

}
