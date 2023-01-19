/// A placeholder representing a semantically erroneous expression in the AST.
public struct ErrorExpr: Expr {

  public let origin: SourceRange

  public init(origin: SourceRange) {
    self.origin = origin
  }

}
