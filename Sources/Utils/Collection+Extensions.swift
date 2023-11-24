extension Collection {

  /// Accesses the unique element of the collection.
  ///
  /// This property is `nil` unless `self.count == 1`.
  public var uniqueElement: Element? {
    count == 1 ? self[startIndex] : nil
  }

  /// The first element of `self` and its suffix after its first index or `nil` if `self` is empty.
  public var headAndTail: (head: Element, tail: SubSequence)? {
    if isEmpty { return nil }
    return (head: self[startIndex], tail: self[index(after: startIndex)...])
  }

  /// Returns the index of the first element in the collection that matches the predicate.
  ///
  /// The collection must already be partitioned according to the predicate, as if
  /// `self.partition(by: predicate)` had already been called.
  ///
  /// - Complexity: At most log(N) invocations of `predicate`, where N is the length of `self`;
  ///   at most log(N) index offsetting operations if `self` conforms to `RandomAccessCollection`;
  ///   at most N such operations otherwise.
  public func partitioningIndex(
    where predicate: (Element) throws -> Bool
  ) rethrows -> Index {
    var n = distance(from: startIndex, to: endIndex)
    var l = startIndex

    while n > 0 {
      let half = n / 2
      let mid = index(l, offsetBy: half)
      if try predicate(self[mid]) {
        n = half
      } else {
        l = index(after: mid)
        n -= half + 1
      }
    }
    return l
  }

  /// Returns the minimal elements of `self` using `compare` to order them.
  ///
  /// A minimal element of a set *S* with a strict partial order *R* is an element is not smaller
  /// than any other element in *S*. If *S* is a finite set and *R* is a strict total order, the
  /// notions of minimal element and minimum coincide.
  ///
  /// - Complexity: O(*n*^2) where *n* is the length of `self`.
  public func minimalElements(
    by compare: (Element, Element) -> StrictPartialOrdering
  ) -> [Element] {
    if let u = uniqueElement { return [u] }

    // This algorithm successively eliminates elements that are not minimal until all candidates
    // have been considered. All elements are candidates at the start. Then, each candidate is
    // compared with others. Greater elements are moved beyond the end of the candidate list while
    // incomparable ones are left in place. At each point, elements left of the current candidate
    // are known to be incomparable with each others and smaller than eliminated elements.

    var candidates = Array(indices)
    var end = candidates.count
    var i = 0
    var j = 1

    while i < end {
      while j < end {
        switch compare(self[candidates[i]], self[candidates[j]]) {
        case .ascending, .equal:
          candidates.swapAt(j, end - 1)
          end -= 1

        case .descending:
          candidates.swapAt(i, end - 1)
          end -= 1
          j = i + 1

        case nil:
          j += 1
        }
      }

      i += 1
      j = i + 1
    }

    return candidates[0 ..< end].map({ self[$0] })
  }

}

extension Collection where Index == Int {

  /// Returns the index `i` that partitions `self` in two halves so that elements in `self[..<i]`
  /// and `self[i...]` are ordered before and after `pivot`, respectively.
  ///
  /// - Complexity: O(log *n*) where *n* is the length of `self`.
  /// - Requires: For any pair of indices `(i, j)` in `self`, `i < j` implies that
  ///   `areInIncreasingOrder(self[i], self[j])` is true.
  /// - Returns: `self.firstIndex(where: { areInIncreasingOrder(pivot, e) })`
  public func partitioningIndex(
    at pivot: Element,
    orderedBy areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows -> Int {
    var lower = 0
    var upper = count

    while lower < upper {
      let middle = (lower + upper) / 2
      if try areInIncreasingOrder(pivot, self[middle]) {
        upper = middle
      } else {
        lower = middle + 1
      }
    }

    return lower
  }

}

extension Collection where Index == Int, Element: Comparable {

  /// Returns the index `i` that partitions `self` in two halves so that elements in `self[..<i]`
  /// and `self[i...]` are ordered before and after `pivot`, respectively.
  ///
  /// If `pivot` is contained in `self`, this method returns the index after that of last
  /// occurrence of `pivot`.
  ///
  /// - Complexity: O(log *n*) where *n* is the length of `self`.
  /// - Requires: For any pair of indices `(i, j)` in `self`, `i < j` implies `self[i] < self[j]`.
  /// - Returns: `self.firstIndex(where: { pivot < e }) ?? self.endIndex`
  public func partitioningIndex(at pivot: Element) -> Int {
    partitioningIndex(at: pivot, orderedBy: <)
  }

}

extension BidirectionalCollection {

  /// Returns `self` sans any suffix elements satisfying `predicate`.
  public func dropLast(while predicate: (Element) throws -> Bool) rethrows -> Self.SubSequence {
    let head = try self.reversed().drop(while: predicate)
    return self[head.endIndex.base ..< head.startIndex.base]
  }

  /// Returns the slice of self that remains after dropping leading and trailing whitespace.
  public func strippingWhitespace() -> SubSequence
  where Element == Character {
    self.drop(while: { $0.isWhitespace }).dropLast(while: { $0.isWhitespace })
  }

}

extension BidirectionalCollection where Element: Equatable {

  /// Returns `(head: a, tail: b)` where `a` is the prefix up to the last occurrence of `element`
  /// and `b` contains the remaining elements. `a` is empty if `element` is not in `self`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the collection.
  public func split(atLastIndexOf element: Element) -> (head: SubSequence, tail: SubSequence) {
    let i = lastIndex(of: element) ?? startIndex
    return (prefix(upTo: i), suffix(from: i))
  }

}

extension MutableCollection {

  /// Rotates the order of the elements in range `r` in such a way that the element at `m` becomes
  /// the first element in `r`, returning the new position of the previously first element in `r`.
  ///
  /// For example:
  ///
  ///      var s = Array(0 ..< 5)           // [0, 1, 2, 3, 4]
  ///      s.rotate(0 ..< 4, toStartAt: 3)  // [3, 0, 1, 2, 4]
  ///
  /// - Requires: `r` is valid in `self` and `m` is contained in `r`.
  /// - Complexity: O(*n*), where *n* is the length of the `r`.
  @discardableResult
  public mutating func rotate(_ r: Range<Index>, toStartAt m: Index) -> Index {
    precondition(r.contains(m))
    if m == r.lowerBound {
      return m
    }

    var n = r.lowerBound
    var write = r.lowerBound
    var read = m
    while read != r.upperBound {
      if write == n { n = read }
      swapAt(write, read)
      write = index(after: write)
      read = index(after: read)
    }

    if write != r.upperBound {
      rotate(write ..< r.upperBound, toStartAt: n)
    }
    return write
  }

  /// Rotates the order of the elements in `self` in such a way that the element at `m` becomes the
  /// first element, returning the position of the previously first element.
  ///
  /// For example:
  ///
  ///      var s = Array(0 ..< 5)  // [0, 1, 2, 3, 4]
  ///      s.rotate(toStartAt: 3)  // [3, 4, 0, 1, 2]
  ///
  /// - Requires: `m` is a valid index in `self` less than `endIndex`.
  /// - Complexity: O(*n*), where *n* is the length of the collection.
  @discardableResult
  public mutating func rotate(toStartAt m: Index) -> Index {
    rotate(startIndex ..< endIndex, toStartAt: m)
  }

}

extension RangeReplaceableCollection {

  /// Filters this collection to keep only the elements satisfying `isIncluded`.
  public mutating func filterInPlace(_ isIncluded: (Element) throws -> Bool) rethrows {
    try removeAll(where: { (e) in try !isIncluded(e) })
  }

}

extension Collection where Self == Self.SubSequence {

  /// Removes `prefix` from the beginning and returns `true`, or returns `false` if `self`
  /// has no such prefix.
  public mutating func removeLeading<P: Collection>(_ prefix: P) -> Bool
  where P.Element == Element, Element: Equatable {
    var me = self[...]
    var p = prefix[...]

    while let x = me.first, let y = p.first {
      if x != y { return false }
      me = me.dropFirst()
      p = p.dropFirst()
    }

    if !p.isEmpty { return false }
    self = me
    return true
  }

  /// Removes elements from the beginning until the first element satisfies `shouldBeKept` or `self`
  /// is empty, returning the removed subsequence.
  @discardableResult
  public mutating func removeFirstUntil(it shouldBeKept: (Element) -> Bool) -> Self {
    return removeFirstWhile(it: { !shouldBeKept($0) })
  }

  /// Removes elements from the beginning that satisfy `shouldBeRemoved`, returning the removed
  /// subsequence.
  @discardableResult
  public mutating func removeFirstWhile(it shouldBeRemoved: (Element) -> Bool) -> Self {
    let t = self.drop(while: shouldBeRemoved)
    defer { self = t }
    return self[..<t.startIndex]
  }

}
