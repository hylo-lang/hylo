extension Array where Element: Equatable {

  /// Appends `element` to `self` iff it is not already contained in `self`, returning the index
  /// where the element is located.
  ///
  /// - Complexity: O(n) where n is the length of `self`.
  public mutating func appendUnlessContained(_ element: Element) -> Int {
     if let index = self.firstIndex(of: element) {
      return index
    } else {
      self.append(element)
      return self.count - 1
    }
  }

  /// Appends the elements `elements` to `self` iff they are not already contained in `self`.
  ///
  /// - Complexity: O(m*n) where m is the length of `elements` and n is the length of `self`.
  public mutating func appendUnlessContained<S: Sequence>(contentsOf elements: S)
  where S.Element == Element {
    for element in elements {
      if !self.contains(element) {
        self.append(element)
      }
    }
  }

}
