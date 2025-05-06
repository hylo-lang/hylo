/// The traits of a refinement closure sorted in topological order w.r.t. to their refinements.
///
/// The sequence is ordered such that a trait `t` always occur **after** the traits which it
/// refines. Hence the last element of the sequence is the most refined trait of the closure.
struct RefinementSequence: IteratorProtocol, Sequence, Sendable {

  /// A stack of vertices left to visit with the indices of their currently visited neighbors.
  private typealias VisitList = [(TraitType, RefinementClosure.Refinements.OutgoingEdges.Index)]

  /// The closure from which traits are returned.
  let closure: RefinementClosure

  /// The current state of the iterator.
  private var state: VisitList

  /// The traits that have been returned already.
  private var visited: Set<TraitType>

  /// Creates an instance returning the traits in `closure`.
  init(_ closure: RefinementClosure) {
    self.closure = closure
    self.state = [(closure.bottom, closure.refinements[from: closure.bottom].startIndex)]
    self.visited = []
    self.visited.reserveCapacity(closure.unordered.count)
  }

  /// The number of traits in `closure`.
  var underestimatedCount: Int {
    closure.unordered.count
  }

  /// Returns the next trait, or `nil` if all traits have been returned already.
  mutating func next() -> TraitType? {
    while let (t, i) = state.last {
      let parents = closure.refinements[from: t]

      if i == parents.endIndex {
        state.removeLast()
        if let (u, j) = state.last {
          state[state.endIndex - 1].1 = closure.refinements[from: u].index(after: j)
        }
        if visited.insert(t).inserted {
          return t
        } else {
          continue
        }
      }

      let p = parents[i].key
      let k = closure.refinements[from: parents[i].key].startIndex
      state.append((p, k))
    }

    return nil
  }

}
