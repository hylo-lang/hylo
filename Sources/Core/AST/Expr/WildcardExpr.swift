/// A wildcard expression.
public struct WildcardExpr: Expr {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
