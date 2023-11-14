import Core

/// The traits of a refinement cluster sorted in topological order w.r.t. to their refinements.
///
/// The order of the traits in the sequence is such that the refinements of a trait always occur
/// before that trait. In other words, if `t` occurs after `u`, then `t` does not refine `u`.
struct RefinementIterator: IteratorProtocol, Sequence {

  /// A stack of vertices left to visit with the indices of their currently visited neighbors.
  private typealias VisitList = [(TraitType, RefinementCluster.Refinements.OutgoingEdges.Index)]

  /// The cluster from which traits are returned.
  let cluster: RefinementCluster

  /// The current state of the iterator.
  private var state: VisitList

  /// The traits that have been returned already.
  private var visited: Set<TraitType>

  /// Creates an instance returning the traits in `cluster`.
  init(_ cluster: RefinementCluster) {
    self.cluster = cluster
    self.state = [(cluster.bottom, cluster.refinements[from: cluster.bottom].startIndex)]
    self.visited = []
    self.visited.reserveCapacity(cluster.unorderedTraits.count)
  }

  /// The number of traits in `cluster`.
  var underestimatedCount: Int {
    cluster.unorderedTraits.count
  }

  /// Returns the next trait, or `nil` if all traits have been returned already.
  mutating func next() -> TraitType? {
    while let (t, i) = state.last {
      let parents = cluster.refinements[from: t]

      if i == parents.endIndex {
        state.removeLast()
        if let (u, j) = state.last {
          state[state.endIndex - 1].1 = cluster.refinements[from: u].index(after: j)
        }
        if visited.insert(t).inserted {
          return t
        } else {
          continue
        }
      }

      let p = parents[i].key
      let k = cluster.refinements[from: parents[i].key].startIndex
      state.append((p, k))
    }

    return nil
  }

}
