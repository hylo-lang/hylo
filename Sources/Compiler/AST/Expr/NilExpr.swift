/// A nil expression.
public struct NilExpr: Expr {

  public let origin: SourceRange?

  public init(origin: SourceRange?) {
    self.origin = origin
  }

}
