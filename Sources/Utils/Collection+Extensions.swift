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
