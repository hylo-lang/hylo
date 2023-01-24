/// An expression evaluated in place.
public struct InoutExpr: Expr {

  public let site: SourceRange

  /// The site of the `&` operator.
  public let operatorSite: SourceRange

  /// The underlying expression.
  public let subject: AnyExprID

  public init(operatorSite: SourceRange, subject: AnyExprID, site: SourceRange) {
    self.site = site
    self.operatorSite = operatorSite
    self.subject = subject
  }

}
