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

  /// Returns the number of elements for which `predicate` holds.
  ///
  /// - Complexity: O(*n*) where *n* is the length of `self`.
  /// - Returns: `self.filter(predicate).count`
  public func elementCount(where predicate: (Element) throws -> Bool) rethrows -> Int {
    try reduce(0, { (s, e) in try predicate(e) ? s + 1 : s })
  }

  /// Returns an array containing the results of `transform` applied on a mutable projection of
  /// `environment` and each of the sequence’s elements.
  public func map<E, T>(
    mutating environment: inout E, _ transform: (inout E, Element) throws -> T
  ) rethrows -> [T] {
    try modified([]) { (transformed) in
      transformed.reserveCapacity(underestimatedCount)
      for e in self {
        try transformed.append(transform(&environment, e))
      }
    }
  }

  /// Returns an array containing the non-`nil` results of `transform` applied on a mutable
  /// projection of `environment` and each of the sequence’s elements.
  public func compactMap<E, T>(
    mutating environment: inout E, _ transform: (inout E, Element) throws -> T?
  ) rethrows -> [T] {
    try modified([]) { (transformed) in
      transformed.reserveCapacity(underestimatedCount)
      for e in self {
        if let x = try transform(&environment, e) { transformed.append(x) }
      }
    }
  }

}
