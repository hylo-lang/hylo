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

  /// Returns the indices of the start of each line, in order.
  public func lineBoundaries() -> [Index] {
    var r = [startIndex]
    var remainder = self[...]
    while !remainder.isEmpty, let i = remainder.firstIndex(of: "\n") {
      let j = index(after: i)
      r.append(j)
      remainder = remainder[j...]
    }
    return r
  }

  /// Replace any sucession of whitespace characters (including newlines and tabs) with a single
  /// white space charater.
  public func canonicalize() -> String {
    var remaining = self[..<self.endIndex]
    var result: String = ""
    while !remaining.isEmpty {
      // Skip all the whitespaces
      remaining = remaining.drop(while: { $0.isWhitespace })
      if !result.isEmpty {
        // The skipped whitespaces will be encoded as a single whitespace
        result.append(" ")
      }

      // Now, `remaining` starts with a non-whitespace
      // Find the next whitespace, i.e., continous non-whitespace text
      let idx = remaining.firstIndex(where: { $0.isWhitespace }) ?? self.endIndex

      // Copy non-whitespace text, and advance the text we need to copy
      result.append(contentsOf: remaining[..<idx])
      remaining = remaining[idx ..< self.endIndex]
    }

    return result
  }
}
