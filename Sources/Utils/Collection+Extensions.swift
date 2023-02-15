extension Collection {

  /// Accesses the unique element of the collection.
  ///
  /// This property is `nil` unless `self.count == 1`.
  public var uniqueElement: Element? {
    count == 1 ? self[startIndex] : nil
  }

  /// The first element of `self` and its suffix after its first index or `nil` if `self` is empty.
  public var headAndTail: (Element, SubSequence)? {
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

extension RangeReplaceableCollection {

  /// Filters this collection to keep only the elements satisfying `isIncluded`.
  public mutating func filterInPlace(_ isIncluded: (Element) throws -> Bool) rethrows {
    try removeAll(where: { (e) in try !isIncluded(e) })
  }

}
