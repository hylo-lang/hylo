/// A nil literal expression.
public struct NilLiteralExpr: Expr {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
