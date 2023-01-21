extension BidirectionalCollection {

  /// Returns `self` sans any suffix elements satisfying `predicate`.
  public func dropLast(while predicate: (Element) throws -> Bool) rethrows -> Self.SubSequence {
    let head = try self.reversed().drop(while: predicate)
    return self[head.endIndex.base ..< head.startIndex.base]
  }

  /// Returns the slice of self that remains after dropping leading and trailing whitespace.
  public func strippingWhitespace() -> SubSequence
  where Element == Character {
    return self.drop { c in c.isWhitespace }
      .dropLast { c in c.isWhitespace }
  }

}

extension Int {

  /// Returns `self` rounded up to the next power of two.
  ///
  /// - Requires: `self` must be a positive integer.
  public var roundedUpToNextPowerOfTwo: Int {
    var x = UInt(bitPattern: self &- 1)
    x |= x &>> 1
    x |= x &>> 2
    x |= x &>> 4
    x |= x &>> 8
    x |= x &>> 16
    #if (arch(x86_64) || arch(arm64))
      x |= x &>> 32
    #elseif (!arch(i386) && !arch(arm))
      if Int.bitWidth > 32 {
        x |= x &>> 32
      }
    #endif
    return Int(bitPattern: x &+ 1)
  }

}

extension Collection {
  /// Returns the index of the first element in the collection
  /// that matches the predicate.
  ///
  /// The collection must already be partitioned according to the
  /// predicate, as if `self.partition(by: predicate)` had already
  /// been called.
  ///
  /// - Efficiency: At most log(N) invocations of `predicate`, where
  ///   N is the length of `self`.  At most log(N) index offsetting
  ///   operations if `self` conforms to `RandomAccessCollection`;
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
