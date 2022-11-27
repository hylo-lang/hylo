/// A wildcard type expression.
public struct WildcardTypeExpr: TypeExpr {

  public let origin: SourceRange?

  public init(origin: SourceRange?) {
    self.origin = origin
  }

}
