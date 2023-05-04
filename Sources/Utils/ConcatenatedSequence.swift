/// The concatenation of two sequences.
public struct ConcatenatedSequence<A: Sequence, B: Sequence>: Sequence
where A.Element == B.Element {

  /// The type of this sequence's elements.
  public typealias Element = A.Element

  /// An iterator over the concatenation of two sequences.
  public struct Iterator: IteratorProtocol {

    /// The iterator over the first sequence.
    fileprivate var head: A.Iterator

    /// The iterator over the second sequence.
    fileprivate var tail: B.Iterator

    /// Returns the next element, or `nil` if no such element exists.
    public mutating func next() -> Element? {
      head.next() ?? tail.next()
    }

  }

  /// The first sequence.
  private let head: A

  /// The second sequence.
  private let tail: B

  /// Creates a sequence with the elements of `head` and `tail` concatenated.
  public init(_ head: A, _ tail: B) {
    self.head = head
    self.tail = tail
  }

  /// A value less than or equal to the number of elements in the sequence, calculated
  /// nondestructively.
  public var underestimatedCount: Int {
    head.underestimatedCount + tail.underestimatedCount
  }

  /// Returns an iterator over the contents of `self`.
  public func makeIterator() -> Iterator {
    Iterator(head: head.makeIterator(), tail: tail.makeIterator())
  }

}

extension Sequence {

  /// Returns the elements of this sequence concatenated with the elements in `tail`.
  public func concatenated<Tail>(with tail: Tail) -> ConcatenatedSequence<Self, Tail> {
    ConcatenatedSequence(self, tail)
  }

  /// Returns the concatenation of `head` and `tail`.
  public static func ++ <Tail>(head: Self, tail: Tail) -> ConcatenatedSequence<Self, Tail> {
    ConcatenatedSequence(head, tail)
  }

}

extension ConcatenatedSequence: Equatable where A: Equatable, B: Equatable {}

extension ConcatenatedSequence: Hashable where A: Hashable, B: Hashable {}

/// The concatenation of two collections.
public typealias ConcatenatedCollection<A: Collection, B: Collection> =
  ConcatenatedSequence<A, B> where A.Element == B.Element

extension ConcatenatedCollection: Collection {

  /// An index in the concatenation of two collections.
  public enum Index: Comparable {

    /// An index in the first collection
    case prefix(A.Index)

    /// An index in the second collection.
    case suffix(B.Index)

    /// Returns `true` if `l` precedes `r`.
    public static func < (l: Index, r: Index) -> Bool {
      switch (l, r) {
      case (.prefix, .suffix):
        return true
      case (.prefix(let a), .prefix(let b)):
        return a < b
      case (.suffix, .prefix):
        return false
      case (.suffix(let a), .suffix(let b)):
        return a < b
      }
    }

  }

  /// The position of the first element.
  public var startIndex: Index {
    .prefix(head.startIndex)
  }

  /// The position of the last element.
  public var endIndex: Index {
    .suffix(tail.endIndex)
  }

  /// Returns the position immediately after `position`.
  public func index(after position: Index) -> Index {
    switch position {
    case .prefix(let i):
      let j = head.index(after: i)
      return j != head.endIndex ? .prefix(j) : .suffix(tail.startIndex)
    case .suffix(let i):
      return .suffix(tail.index(after: i))
    }
  }

  /// Accesses the element at `position`.
  public subscript(position: Index) -> Element {
    switch position {
    case .prefix(let i):
      return head[i]
    case .suffix(let i):
      return tail[i]
    }
  }

}
