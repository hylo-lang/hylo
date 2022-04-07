/// A conformance lens.
public struct ConformanceLensTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The expression of the lens' base.
  public var base: TypeExpr

  /// The name of the trait in which the lens focuses.
  public var trait: Identifier

  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(conformanceLens: self)
  }

}
