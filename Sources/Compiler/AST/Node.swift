/// A protocol describing the API of an AST node.
public protocol Node: Codable {

  /// A unique identifier denoting the type of this node.
  static var kind: NodeKind { get }

  /// Returns `.success` if `self` is well-formed given the containing `ast`. Otherwise, returns
  /// `.failure` with the diagnostics of the broken invariants.
  func checkInvariants(in ast: AST) -> FallibleWithDiagnostic<Void>

}

extension Node {

  public func checkInvariants(in ast: AST) -> FallibleWithDiagnostic<Void> { .success(()) }

}
