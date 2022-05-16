import LoftDataStructures_Zip2Collection

extension BidirectionalCollection {
  /// Returns `self` sans any suffix elements satisfying `predicate`.
  public func dropLast(while predicate: (Element) throws -> Bool) rethrows -> Self.SubSequence {
    let head = try self.reversed().drop(while: predicate)
    return self[head.endIndex.base..<head.startIndex.base]
  }
}

extension StringProtocol {
  /// Returns the slice of self that remains after dropping leading and trailing whitespace.
  public func strippingWhitespace() -> SubSequence
    where Element == Character
  {
    return self.drop { c in c.isWhitespace }
      .dropLast { c in c.isWhitespace }
  }

  /// Returns the lines from markdown fenced code blocks in `self` (having the
  /// given language if `language` is non-`nil`), paired with their line numbers.
  public func markdownCodeLines(language: String? = nil)
    -> [Slice<Zip2Collection<Range<Int>, [Self.SubSequence]>>]
  {
    let allLines = self.split { c in c.isNewline }.enumerated()
    let fence = "```" + (language ?? "")
    return allLines.subSequencesBetween(
      elementsSatisfying: { l in l.1.strippingWhitespace() == fence },
      and: { l in l.1.strippingWhitespace() == fence.prefix(3) }
    )
  }
}

extension Collection {
  /// Returns the descriptions of all elements, joined by the given `separator`.
  public func descriptions(joinedBy separator: String = ", ") -> String {
    self.lazy.map(String.init(describing:)).joined(separator: separator)
  }
}

extension Swift.Collection {
  /// Returns the `SubSequence`s between pairs of elements bounded by an element
  /// satisfying `isOpenDelimiter` as an open deliminter and `isCloseDelimiter`
  /// as a closing delimiter.
  ///
  /// - Precondition: elements satisfying the two predicates strictly alternate,
  ///   starting with `isOpenDelimiter`.
  public func subSequencesBetween(
    elementsSatisfying isOpenDelimiter: (Element)->Bool,
    and isCloseDelimiter: (Element)->Bool
  ) -> [SubSequence] {
    var r: [SubSequence] = []
    var openDelimiterPosition: Index? = nil
    for i in self.indices {
      if let o = openDelimiterPosition {
        if isCloseDelimiter(self[i]) {
          r.append(self[index(after: o)..<i])
          openDelimiterPosition = nil
        }
      }
      else {
        if isOpenDelimiter(self[i]) {
          openDelimiterPosition = i
        }
      }
    }
    return r
  }
}
