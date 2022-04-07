/// A declaration.
public protocol Decl {

  /// Accepts the specified visitor.
  func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result

}
