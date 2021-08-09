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

extension Dictionary {

  /// Creates a dictionary by merging key-value pairs in a sequence into the dictionary.
  ///
  /// - Parameter keysAndValues: A sequence of key-value pairs.
  public func merging<S>(disjointKeysWithValues keysAndValues: S) -> Dictionary
  where S: Sequence, S.Element == (Key, Value)
  {
    var d = self
    for (key, value) in keysAndValues {
      precondition(d[key] == nil, "duplicate key '\(key)'")
      d[key] = value
    }
    return d
  }

}

extension Sequence {

  /// Returns the first element in the sequence that has the specified type.
  public func first<T>(as elementType: T.Type) -> T? {
    for case let element as T in self {
      return element
    }
    return nil
  }

}

infix operator ?< : NilCoalescingPrecedence

/// Performs a nil-coalescing operation, returning the wrapped value of an Optional instance or
/// calling the specified closure for a default value.
public func ?< <T>(lhs: T?, rhs: () -> T?) -> T? {
  return lhs ?? rhs()
}

/// Performs a nil-coalescing operation, returning the wrapped value of an Optional instance or
/// calling the specified closure for a default value.
public func ?< <T>(lhs: T?, rhs: () -> T) -> T {
  return lhs ?? rhs()
}
