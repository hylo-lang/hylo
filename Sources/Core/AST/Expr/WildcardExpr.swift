/// A wildcard expression.
public struct WildcardExpr: Expr {

  public let origin: SourceRange

  public init(origin: SourceRange) {
    self.origin = origin
  }

}
