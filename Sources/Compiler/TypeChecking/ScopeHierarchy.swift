/// A tree describing the scope hierarchy of an AST.
struct ScopeHierarchy {

  /// A table mapping a lexical scope to its parent.
  var parent = NodeMap<AnyNodeIndex>()

  /// A table mapping a declaration to the innermost lexical scope that contains it.
  var container = DeclMap<AnyNodeIndex>()

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
