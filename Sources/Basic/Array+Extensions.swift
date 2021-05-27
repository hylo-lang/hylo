extension Array {

  /// Returns the index where to insert `element` in the array to keep it sorted, assuming it is
  /// currently sorted.
  ///
  /// - Parameters:
  ///   - element: The element to insert. If a subsequence of equal elements is already contained
  ///     in the array, this method returns the first index before.
  ///   - areInIncreasingOrder: A predicate that returns `true` if its first argument should be
  ///     ordered before its second.
  public func sortedInsertionIndex(
    of element: Element,
    sortedBy areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows -> Int {
    var lower = 0
    var upper = count

    while lower < upper {
      let middle = (lower + upper) / 2
      if try areInIncreasingOrder(element, self[middle]) {
        upper = middle
      } else {
        lower = middle + 1
      }
    }

    return lower
  }

}

extension Array where Element: Comparable {

  /// Returns the index where to insert `element` in the array to keep it sorted, assuming it is
  /// currently sorted.
  ///
  /// - Parameter element: The element to insert. If a subsequence of equal elements is already
  ///   contained in the array, this method returns the first index before.
  public func sortedInsertionIndex(of element: Element) -> Int {
    sortedInsertionIndex(of: element, sortedBy: <)
  }

}
