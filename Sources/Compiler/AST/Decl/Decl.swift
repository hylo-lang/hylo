/// A declaration.
public protocol Decl {

  /// The source range of the declaration's textual representation, if any.
  var range: SourceRange? { get }

  /// Accepts the specified visitor.
  func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result

}
