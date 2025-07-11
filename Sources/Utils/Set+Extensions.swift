extension Set {

  /// Removes all the elements that satisfy `predicate` from `self`.
  public mutating func removeAll(where predicate: @Sendable (Element) throws -> Bool) rethrows {
    self = try self.filter(predicate)
  }

  // Removes all the elements that satisfy `predicate` from `self`, and provides a mutable state to the predicate.
  public mutating func removeAll<S>(withState state: inout S, where predicate: (inout S, Element) throws -> Bool) rethrows {
    self = try self.filter { element in
      try !predicate(&state, element)
    }
  }

  /// Returns a set containing the elements in `self` and `newElement`.
  public func inserting(_ newElement: Element) -> Self {
    var result = self
    result.insert(newElement)
    return result
  }

  public func contains<S>(withState state: inout S, where predicate: (S, Element) throws -> Bool) rethrows -> Bool {
    try self.contains { element in
      try predicate(state, element)
    }
  }

}
