extension Dictionary where Value: Equatable {

  /// Removes the key/value pairs in `self` that are not also in `other`.
  public mutating func formIntersection(_ other: Self) {
    for (key, lhs) in self {
      self[key] = (lhs == other[key]) ? lhs : nil
    }
  }

  /// Returns a new dictionary containing the key/value pairs common to `self` and `other`.
  public func intersection(_ other: Self) -> Self {
    var result: Self = [:]
    for (key, lhs) in self {
      result[key] = (lhs == other[key]) ? lhs : nil
    }
    return result
  }

}
