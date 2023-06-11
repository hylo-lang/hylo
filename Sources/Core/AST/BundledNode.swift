/// A projection from a `AST` of a node along with its children.
@dynamicMemberLookup
public struct BundledNode<T: NodeIDProtocol> {

  /// The AST of which this node is a notional part.
  private let container: AST

  /// The node's identity in `container`.
  public let id: T

  /// Creates an instance bundling `container` with `id`.
  public init(_ id: T, in container: AST) {
    self.container = container
    self.id = id
  }

  /// The site from which self was parsed.
  public var site: SourceRange {
    container[id].site
  }

}

extension BundledNode where T: ConcreteNodeID {

  /// Accesses `m` in the corresponding AST node.
  public subscript<Target>(dynamicMember m: KeyPath<T.Subject, Target>) -> Target {
    container[id][keyPath: m]
  }

  /// Accesses `m` in the corresponding AST node.
  public subscript<Target: NodeIDProtocol>(
    dynamicMember m: KeyPath<T.Subject, Target>
  ) -> BundledNode<Target> {
    .init(container[id][keyPath: m], in: container)
  }

}

extension BundledNode: Hashable {

  /// Equality comparison; only check the node ID.
  public static func == (lhs: Self, rhs: Self) -> Bool {
    lhs.id == rhs.id
  }

  /// Hashes the value of `id` into `hasher`.
  public func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }
}

extension BundledNode: CustomStringConvertible {

  public var description: String {
    String(site.text)
  }

}

extension NodeIDProtocol {

  public subscript(in container: AST) -> BundledNode<Self> {
    .init(self, in: container)
  }

}
