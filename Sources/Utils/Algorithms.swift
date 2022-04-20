
extension BidirectionalCollection {
  /// Returns the longest `self[..<j]` such that each element of `self[j...]`
  /// satisfies `predicate`.
  func dropLast(while predicate: (Element) throws -> Bool) rethrows -> Self.SubSequence {
    return self[..<(try self.lastIndex { try !predicate($0) } ?? startIndex)]
  }
}
