/// A wildcard pattern.
public struct WildcardPattern: Pattern {

  public var range: SourceRange?

  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(wildcard: self)
  }

}
