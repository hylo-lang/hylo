/// A graph describing the scope hierarchy of an AST.
struct ScopeHierarchy {

  /// A table mapping a lexical scope to its parent.
  var parent = NodeMap<AnyNodeIndex>()

  /// A table mapping a declaration to the innermost lexical scope that contains it.
  private(set) var container = DeclMap<AnyNodeIndex>()

  /// A table mapping lexical scopes to the declarations directly contained in them.
  private(set) var containees = NodeMap<[AnyDeclIndex]>()

  /// Inserts `decl` into `scope`.
  mutating func insert<T: DeclIndex>(decl: T, into scope: AnyNodeIndex) {
    let child = AnyDeclIndex(decl)

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
  func isContained<T: NodeIndexProtocol, U: NodeIndexProtocol>(
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

}
