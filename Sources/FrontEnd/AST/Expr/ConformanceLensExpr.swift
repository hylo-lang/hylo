/// A conformance lens.
public struct ConformanceLensExpr: Expr {

  public let site: SourceRange

  /// The expression of the subject type.
  public let subject: AnyExprID

  /// The expression of the trait in which the lens focuses.
  public let lens: AnyExprID

  public init(subject: AnyExprID, lens: AnyExprID, site: SourceRange) {
    self.site = site
    self.subject = subject
    self.lens = lens
  }

}
