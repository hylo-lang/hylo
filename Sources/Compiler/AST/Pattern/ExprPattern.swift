/// A pattern that matches the value of an equatable expression.
public struct ExprPattern: Pattern {

  public var range: SourceRange?

  /// The expression of the pattern.
  public var expr: Expr

  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(expr: self)
  }

}
