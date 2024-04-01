import Utils

/// A subset of a program's refinement relation containing all the dependencies of a single trait.
struct RefinementCluster: Equatable {

  /// The refinement relationships of a cluster.
  typealias Refinements = DirectedGraph<TraitType, NoLabel>

  /// The most-refined trait of the cluster.
  let bottom: TraitType

  /// The traits in the cluster, in no particular order.
  private(set) var unordered: Set<TraitType>

  /// The refinement relationships in the cluster.
  private(set) var refinements: Refinements

  /// Creates a cluster containing only `bottom`.
  init(_ bottom: TraitType) {
    self.bottom = bottom
    self.unordered = [bottom]
    self.refinements = .init()
  }

  /// The traits in `self` sorted in topological order w.r.t. to their dependencies.
  var orderedByDependency: RefinementSequence { .init(self) }

  /// Returns `true` iff `t` is in the cluster.
  ///
  /// - Complexity: O(1)
  func contains(_ t: TraitType) -> Bool {
    unordered.contains(t)
  }

  /// Inserts `t` as a trait inherited by `u` in `self`.
  ///
  /// - Requires: `u` is in the cluster and, if `t != u`, then `t` isn't refined by any trait in
  ///   the cluster.
  mutating func insert(_ t: TraitType, inheritedBy u: TraitType) {
    if t == u { return }
    assert(!refinements.isReachable(u, from: t), "refinement cycle")
    refinements.insertEdge(from: u, to: t)
    unordered.insert(t)
  }

  /// Inserts the contents of `c` as traits inherited by `u` in `self`.
  ///
  /// - Requires: `c.bottom` is in the cluster.
  mutating func insert(_ c: Self, inheritedBy u: TraitType) {
    var work = [(c.bottom, u)]
    while let (s, t) = work.popLast() {
      insert(s, inheritedBy: t)
      work.append(contentsOf: c.refinements[from: s].map({ (e) in (e.key, s) }))
    }
  }

}
