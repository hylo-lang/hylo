extension Set {

  /// Removes all the elements that satisfy `predicate` from `self`.
  public mutating func removeAll(where predicate: (Element) throws -> Bool) rethrows {
    self = try self.filter(predicate)
  }

}
