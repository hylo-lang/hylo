/// A tree describing the scope hierarchy of a module.
struct ScopeHierarchy {

  /// A table mapping a lexical scope to its parent.
  var parent: [ScopeID: ScopeID] = [:]

  /// Returns whether `child` is contained in `ancestor`.
  ///
  /// - Requires: `child` is the identifier of a scope in this hierarchy.
  func isContained(_ child: ScopeID, in ancestor: ScopeID) -> Bool {
    var current = child
    while true {
      if child == current {
        return true
      } else if let p = parent[current] {
        current = p
      } else {
        return false
      }
    }
  }

}
