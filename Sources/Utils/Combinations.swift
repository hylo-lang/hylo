/// A collection containing all possible combinations of elements in another collection.
public struct Combinations<Base: Collection> {

  /// The collection from which `self`'s elements are picked.
  public let base: Base

  /// The number of elements in each combination.
  public let combinationCount: Int

  /// Creates a collection of with all possible combinations of `count` elements in `base`.
  ///
  /// - Requires: `base.count >= count`.
  public init(_ base: Base, of count: Int) {
    precondition(base.count >= count)
    self.base = base
    self.combinationCount = count
  }

}

extension Combinations: Collection {

  /// A position in this collection.
  public struct Index: Comparable {

    /// The positions of each element in the combination.
    fileprivate let positions: [Base.Index]?

    /// Returns `true` iff `l` is strictly lesser than `r`.
    public static func < (l: Self, r: Self) -> Bool {
      guard let lhs = l.positions else { return false }
      guard let rhs = r.positions else { return true }
      for (a, b) in zip(lhs, rhs) where a != b {
        return a < b
      }
      return false
    }

  }

  /// A Boolean value indicating whether the collection is empty.
  public var isEmpty: Bool {
    false
  }

  /// The number of elements in `Self`.
  public var count: Int {
    let n = base.count
    let k = combinationCount
    return n.factorial / (k.factorial * (n - k).factorial)
  }

  /// A value less than or equal to the number of elements in the collection.
  public var underestimatedCount: Int {
    count
  }

  /// Accesses the combination at `i`.
  public subscript(i: Index) -> [Base.Element] {
    i.positions!.map({ base[$0] })
  }

  /// The index of the first combination.
  public var startIndex: Index {
    .init(positions: Array(base.indices.prefix(combinationCount)))
  }

  /// The collection's "past the end" position.
  public var endIndex: Index {
    .init(positions: nil)
  }

  /// Returns the index immediately after `i`.
  ///
  /// - Requires: `i` is an index in `self` different from `endIndex`.
  public func index(after i: Index) -> Index {
    var positions = i.positions!

    // Handle k = 0.
    if positions.isEmpty {
      return endIndex
    }

    // Identify the next combination.
    var end = base.endIndex
    for j in (0 ..< combinationCount).reversed() {
      let advanced = base.index(after: positions[j])
      if advanced != end {
        update(positions: &positions, from: j, toStartAt: advanced)
        return .init(positions: positions)
      } else {
        end = positions[j]
      }
    }

    // End of the sequence.
    return endIndex
  }

  /// Assigns `positions[j...]` to a sequence of consecutive base indices starting from `p`.
  private func update(positions: inout [Base.Index], from j: Int, toStartAt p: Base.Index) {
    var p = p
    for k in positions[j...].indices {
      positions[k] = p
      p = base.index(after: p)
    }
  }

}

extension Combinations: Equatable where Base: Equatable {}

extension Combinations: Hashable where Base: Hashable {}

extension Collection {

  /// Returns all possible combinations of `count` elements picked from `self`.
  public func combinations(of count: Int) -> Combinations<Self> {
    .init(self, of: count)
  }

}
