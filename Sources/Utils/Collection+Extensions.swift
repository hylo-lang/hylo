extension Collection {

  /// Returns an array containing the parts of the elements in `self` accessed by `path`.
  public func map<T>(_ path: KeyPath<Element, T>) -> [T] {
    map({ (e) in e[keyPath: path] })
  }

  /// Accesses the unique element of the collection.
  ///
  /// This property is `nil` unless `self.count == 1`.
  public var uniqueElement: Element? {
    count == 1 ? self[startIndex] : nil
  }

}

extension RangeReplaceableCollection {

  /// Filters this collection to keep only the elements satisfying `isIncluded`.
  public mutating func filterInPlace(_ isIncluded: (Element) throws -> Bool) rethrows {
    try removeAll(where: { (e) in try !isIncluded(e) })
  }

}
