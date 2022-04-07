/// A pattern.
public protocol Pattern: SourceRepresentable {

  /// Accepts the specified visitor.
  func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result

}
