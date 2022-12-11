/// A nil literal expression.
public struct NilLiteralExpr: Expr {

  public let origin: SourceRange?

  public init(origin: SourceRange?) { self.origin = origin }

}
