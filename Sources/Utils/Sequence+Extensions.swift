extension Sequence {

  /// Returns the descriptions of all elements, joined by the given `separator`.
  public func descriptions(joinedBy separator: String = ", ") -> String {
    var result = ""
    var first = true
    for x in self {
      if first { first = false } else { result.append(separator) }
      result.append(String(describing: x))
    }
    return result
  }

  /// The result of the first scucessful transformation applied to elements in `self`.
  public func first<T>(transformedBy transform: (Element) throws -> T?) rethrows -> T? {
    for x in self {
      if let y = try transform(x) { return y }
    }
    return nil
  }

}
