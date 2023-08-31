/// A projection from a `Program` of a node along with its extrinsic relationships.
@dynamicMemberLookup
public struct BundledNode<T: NodeIDProtocol, P: Program> {

  /// The program of which this node is a notional part.
  ///
  /// FIXME: make me private.
  public let container: P

  /// The node's identity in `container.ast`.
  public let id: T

  /// Creates an instance bundling `container` with `id`.
  public init(_ id: T, in container: P) {
    self.container = container
    self.id = id
  }

  /// The site from which self was parsed.
  public var site: SourceRange {
    container.ast[id].site
  }

  /// The scope in which the node resides.
  ///
  /// - Requires: `self` does not identify a module.
  public var scope: AnyScopeID {
    container.nodeToScope[id]!
  }

}

extension BundledNode where T: ConcreteNodeID {

  /// Accesses `m` in the corresponding AST node.
  public subscript<Target>(dynamicMember m: KeyPath<T.Subject, Target>) -> Target {
    container.ast[id][keyPath: m]
  }

  /// Accesses `m` in the corresponding AST node.
  public subscript<Target: NodeIDProtocol>(
    dynamicMember m: KeyPath<T.Subject, Target>
  ) -> BundledNode<Target, P> {
    .init(container.ast[id][keyPath: m], in: container)
  }

}

extension BundledNode where T: ScopeID {

  /// The declarations in this immediate scope.
  public var decls: [AnyDeclID] {
    container.scopeToDecls[id, default: []]
  }

}

extension BundledNode where T == VarDecl.ID {

  /// The binding declaration containing this variable declaration.
  public var binding: BindingDecl.ID {
    container.varToBinding[id]!
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
