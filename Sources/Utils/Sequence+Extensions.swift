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

}
