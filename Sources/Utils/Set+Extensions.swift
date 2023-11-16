extension Set {

  /// Removes all the elements that satisfy `predicate` from `self`.
  public mutating func removeAll(where predicate: (Element) throws -> Bool) rethrows {
    self = try self.filter(predicate)
  }

  /// Returns a set containing the elements in `self` and `newElement`.
  public func inserting(_ newElement: Element) -> Self {
    var result = self
    result.insert(newElement)
    return result
  }

}
