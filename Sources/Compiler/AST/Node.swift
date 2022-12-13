/// A protocol describing the API of an AST node.
public protocol Node: Codable {

  /// The source range from which `self` was parsed, if any.
  var origin: SourceRange? { get }

  /// Returns `.success` if `self` is well-formed given the containing `ast`. Otherwise, returns
  /// `.failure` with the diagnostics of the broken invariants.
  func validateForm(in ast: AST) -> SuccessOrDiagnostics

}

extension Node {

  public func validateForm(in ast: AST) -> SuccessOrDiagnostics { .success }

  /// A unique identifier denoting the type of this node.
  static var kind: NodeKind { NodeKind(Self.self) }
}
