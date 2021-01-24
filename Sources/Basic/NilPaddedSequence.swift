public struct NilPaddedSequence<Base> where Base: Sequence {

  public init(_ base: Base) {
    self.base = base
  }

  private let base: Base

}

extension NilPaddedSequence: Sequence {

  public func makeIterator() -> Iterator {
    return Iterator(base: base.makeIterator())
  }

  public struct Iterator: IteratorProtocol {

    fileprivate var base: Base.Iterator

    public mutating func next() -> Base.Element?? {
      return base.next()
    }

  }

}

extension Sequence {

  public var nilPadded: NilPaddedSequence<Self> {
    return NilPaddedSequence(self)
  }

}
