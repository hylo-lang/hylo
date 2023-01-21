/// A protocol to group the CXX node types.
/// The only common API exposed by a CXX node is its kind.
public protocol CXXNode {

  // TODO: will be removed soon
  func writeCode<Target: TextOutputStream>(into target: inout Target)
}

extension CXXNode {

  /// A unique identifier denoting the type of this node.
  static var kind: CXXNodeKind { CXXNodeKind(Self.self) }

}
