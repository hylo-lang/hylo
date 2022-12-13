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

extension Collection {

  /// Returns the descriptions of all elements, joined by the given `separator`.
  public func descriptions(joinedBy separator: String = ", ") -> String {
    self.lazy.map(String.init(describing:)).joined(separator: separator)
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
