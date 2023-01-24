/// A placeholder representing a semantically erroneous expression in the AST.
public struct ErrorExpr: Expr {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
