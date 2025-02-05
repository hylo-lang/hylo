/// A wildcard expression.
public struct WildcardExpr: Expr, Sendable {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
