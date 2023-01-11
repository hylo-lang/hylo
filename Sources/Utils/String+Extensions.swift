extension String.StringInterpolation {

  /// Appends the string descriptions of the elements in `list` separated by `separator`.
  public mutating func appendInterpolation<L: Sequence>(
    list: L,
    joinedBy separator: String = ", "
  ) {
    appendLiteral(list.descriptions(joinedBy: separator))
  }

}

extension StringProtocol {

  /// Removes trailing newlines from the given string subsequence.
  public func removingTrailingNewlines() -> Self.SubSequence {
    if let i = self.lastIndex(where: { !$0.isNewline }) {
      return self.prefix(through: i)
    } else {
      return self.prefix(upTo: startIndex)
    }
  }

  /// Returns the indices of the start of each line, in order, always ending with `endIndex`, even
  /// if there's no final newline.
  public func lineBoundaries() -> [Index] {
    var r = [startIndex]
    var remainder = self[...]
    while !remainder.isEmpty, let i = remainder.firstIndex(of: "\n") {
      let j = index(after: i)
      r.append(j)
      remainder = remainder[j...]
    }
    if r.last != endIndex {
      r.append(endIndex)
    }
    return r
  }

}
