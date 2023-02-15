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
