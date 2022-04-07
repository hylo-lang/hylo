/// A statement.
public protocol Stmt: SourceRepresentable {

  /// Accepts the specified visitor.
  func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result

}
