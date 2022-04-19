extension String {

  /// Concatenates the description of each element in `items`, separated by `separator`.
  public static func joining<S>(_ items: S, separator: String = "") -> String where S: Sequence {
    items.lazy.map(String.init(describing:)).joined(separator: separator)
  }

}
