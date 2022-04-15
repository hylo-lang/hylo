/// A graph describing the scope hierarchy of an AST.
struct ScopeHierarchy {

  /// A sequence of scopes, from inner to outer.
  struct ScopeSequence: IteratorProtocol, Sequence {

    typealias Element = AnyNodeID

    fileprivate let parent: NodeMap<AnyNodeID>

    fileprivate var current: AnyNodeID?

    mutating func next() -> AnyNodeID? {
      guard let s = current else { return nil }
      current = parent[s]
      return s
    }

  }

  /// A table mapping a lexical scope to its parent.
  var parent = NodeMap<AnyNodeID>()

  /// A table mapping a declaration to the innermost lexical scope that contains it.
  private(set) var container = DeclMap<AnyNodeID>()

  /// A table mapping lexical scopes to the declarations directly contained in them.
  private(set) var containees = NodeMap<[AnyDeclID]>()

  /// Inserts `decl` into `scope`.
  mutating func insert<T: DeclID>(decl: T, into scope: AnyNodeID) {
    let child = AnyDeclID(decl)

    if let parent = container[child] {
      if parent == scope {
        // The relation is already established, we're done.
        return
      } else {
        // Remove the existing edge scope container to containee.
        containees[scope]?.removeAll(where: { $0 == child })
      }
    }

    // Create the edges.
    container[child] = scope
    containees[scope, default: []].append(child)
  }

  /// Returns whether `child` is contained in `ancestor`.
  ///
  /// - Requires: `child` is the identifier of a scope in this hierarchy.
  func isContained<T: NodeIDProtocol, U: NodeIDProtocol>(
    _ child: T,
    in ancestor: U
  ) -> Bool {
    var current = child.rawValue
    while true {
      if ancestor.rawValue == current {
        return true
      } else if let p = parent[raw: current] {
        current = p.rawValue
      } else {
        return false
      }
    }
  }

  /// Returns a sequence containing `scope` and all its ancestors, from inner to outer.
  func scopesToRoot(from scope: AnyNodeID) -> ScopeSequence {
    ScopeSequence(parent: parent, current: scope)
  }

}
