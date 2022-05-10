extension String {

  /// Concatenates the description of each element in `items`, separated by `separator`.
  public static func joining<S>(_ items: S, separator: String = "") -> String where S: Sequence {
    items.lazy.map(String.init(describing:)).joined(separator: separator)
  }

  /// Returns the slice of self that remains after dropping leading and trailing whitespace.
  func strippingWhitespace() -> SubSequence {
    return self.drop { c in c.isWhitespace }
      .dropLast { c in c.isWhitespace }
  }

}
