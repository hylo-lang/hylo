extension TextOutputStream {

  /// Appends the contents of `items` stream, writing each element with `writeElement` and
  /// separating them with `separator`.
  public mutating func write<T: Sequence>(
    contentsOf items: T,
    separatedBy separator: String = ",",
    writingElementsWith writeElement: (inout Self, T.Element) throws -> Void
  ) rethrows {
    var isFirst = true
    for e in items {
      if isFirst {
        isFirst = false
      } else {
        write(separator)
      }
      try writeElement(&self, e)
    }
  }

}
