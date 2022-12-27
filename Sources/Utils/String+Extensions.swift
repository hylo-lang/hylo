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

}
