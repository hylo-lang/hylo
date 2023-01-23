/// A protocol describing the API of an AST node.
public protocol Node: Codable {

  /// The source range from which `self` was parsed.
  var site: SourceRange { get }

  /// Reports any well-formedness violations of `self` into `diagnostics`.
  func validateForm(in ast: AST, into diagnostics: inout Diagnostics)

}

extension Node {

  public func validateForm(in ast: AST, into diagnostics: inout Diagnostics) {}

  /// A unique identifier denoting the type of this node.
  static var kind: NodeKind { NodeKind(Self.self) }
}
