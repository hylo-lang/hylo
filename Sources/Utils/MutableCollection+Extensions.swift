extension MutableCollection {

  /// Copies `min(self.count, c.count)` elements from `c` to start of `self`.
  ///
  /// - Complexity: O(`min(self.count, c.count)`).
  public mutating func copyElements<C>(from c: C)
  where
    C: Collection,
    Element == C.Element
  {
    var p = self.startIndex
    for e in c {
      if p == self.endIndex {
        break
      }
      self[p] = e
      self.formIndex(after: &p)
    }
  }

}
