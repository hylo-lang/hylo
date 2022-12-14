extension Collection {

  /// Returns an array containing the parts of the elements in `self` accessed by `path`.
  public func map<T>(_ path: KeyPath<Element, T>) -> [T] {
    map({ (e) in e[keyPath: path] })
  }

}
