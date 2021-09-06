/// An iterator that contatenates the elements produced by multiple iterators.
public struct JoinedIterator<I>: IteratorProtocol where I: IteratorProtocol {

  private var its: [I]

  /// Creates an iterator that contatenates the given iterators.
  ///
  /// - Parameter iterator: A sequence of iterators.
  public init<S>(_ iterators: S) where S: Sequence, S.Element == I {
    its = Array(iterators).reversed()
  }

  public mutating func next() -> I.Element? {
    while !its.isEmpty {
      if let e = its[its.count - 1].next() {
        return e
      } else {
        its.removeLast()
      }
    }
    return nil
  }

}
