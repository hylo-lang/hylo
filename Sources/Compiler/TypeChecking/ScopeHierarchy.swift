/// A graph describing the scope hierarchy of an AST.
struct ScopeHierarchy {

  /// A sequence of scopes, from inner to outer.
  struct ScopeSequence: IteratorProtocol, Sequence {

    typealias Element = AnyScopeID

    fileprivate let parent: NodeMap<AnyScopeID>

    fileprivate var current: AnyScopeID?

    mutating func next() -> AnyScopeID? {
      guard let s = current else { return nil }
      current = parent[s]
      return s
    }

  }

  /// A table mapping a lexical scope to its parent.
  var parent = NodeMap<AnyScopeID>()

  /// A table mapping a declaration to the innermost lexical scope that contains it.
  private(set) var container = DeclMap<AnyScopeID>()

  /// A table mapping lexical scopes to the declarations directly contained in them.
  private(set) var containees = NodeMap<[AnyDeclID]>()

  /// Inserts `decl` into `scope`.
  mutating func insert<T: DeclID>(decl: T, into scope: AnyScopeID) {
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
  /// Scope containment is transitive and reflexive; this method returns `true` if:
  /// - `child == ancestor`, or
  /// - `parent[child] == ancestor`, or
  /// - `isContained(parent[child], ancestor)`.
  ///
  /// - Requires: `child` is the identifier of a scope in this hierarchy.
  func isContained<T: NodeIDProtocol, U: ScopeID>(
    _ child: T,
    in ancestor: U
  ) -> Bool {
    var current = AnyNodeID(child)
    while true {
      if ancestor.rawValue == current.rawValue {
        return true
      } else if let p = parent[current] {
        current = AnyNodeID(p)
      } else {
        return false
      }
    }
  }

  /// Returns a sequence containing `scope` and all its ancestors, from inner to outer.
  func scopesToRoot(from scope: AnyScopeID) -> ScopeSequence {
    ScopeSequence(parent: parent, current: scope)
  }

}
