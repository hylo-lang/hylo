fileprivate let NONE: UInt32 = ~0

/// An ordered container that guarantees stable indices and O(1) insertion and removal.
///
/// Unlike with Swift's `Array`, inserting or removing an element does not invalidate any other
/// index. If `i` is an index in `list`, then `list[i] = e` holds even if `list` is mutated at
/// another index. Further, the data structure guarantees O(1) insertion and deletion in average,
/// at any position.
///
///     var list = StableDoublyLinkedList([2, 3, 4])
///     let i = list.lastIndex
///     list.remove(at: list.startIndex)
///     print(list[i]) // 4
///
/// Use this data structure when you need stable indices (e.g., to store stable identities in other
/// data structures) while retaining the ability to mutate the collection in place, or if you need
/// O(1) insertation and removal.
///
/// Elements are **not** stored in contiguous storage, which might have a significant impact on
/// performance. Further, offsetting indices always require linear time. Hence, you should favor
/// Swift's `Array` if you do not need stable indices and if insertion/removal times do not matter
/// much to you. Alternatively, you may consider using `Dictionary` if you not need ordered indices
/// or wish to use another type.
///
/// Although `StableDoublyLinkedList` does not conform to `Collection`, because its indices are not
/// comparable (see `isPreceeding(_:_:)`), its API is very similar to that of `Array`.
public struct StableDoublyLinkedList<Element> {

  // Implementation note:
  // ====================
  //
  // The internal representation is a doubly linked list in which forward and backward pointers are
  // encoded as offset from the collection's raw storage.
  //
  // The raw storage grows automatically when inserting an element would exceed its capability, but
  // it is never shrunk. The data structure maintains a linked list free cells in the buffer, so
  // that memory can be reused after an element has been removed.
  //
  // Offsets are stored as 32-bit integers and the element of a list link is laid out last, so that
  // a link holding class-bound elements takes two words on a 64-bit machine. The most significant
  // bit is used as a tag to identify the "past the end" index. Hence, a list can contain at most
  // 2^31 elements.

  /// A stable index in a doubly linked list.
  public struct Index: Hashable {

    fileprivate var offset: UInt32

  }

  /// A list that represents a subrange of a list.
  public struct SubList {

    fileprivate let startIndex: Index

    fileprivate let endIndex: Index

    fileprivate let base: StableDoublyLinkedList

  }

  /// A linked list of instructions.
  fileprivate final class List {

    struct Link {

      var next: UInt32

      var prev: UInt32

      var element: Element?

    }

    var baseAddress: UnsafeMutablePointer<Link>?

    var capacity: Int = 0

    var count: Int = 0

    var nextFreeOffset: UInt32 = 0

    var head: UInt32 = NONE

    var tail: UInt32 = NONE

    init() {}

    init(_ buffer: UnsafeBufferPointer<Element>) {
      if buffer.isEmpty { return }

      baseAddress = .allocate(capacity: buffer.count)
      for (i, element) in buffer.enumerated() {
        let next = i < (buffer.count - 1)
          ? UInt32(truncatingIfNeeded: i) + 1
          : NONE
        let prev = i > 0
          ? UInt32(truncatingIfNeeded: i) - 1
          : NONE
        baseAddress!.advanced(by: i).initialize(to: Link(next: next, prev: prev, element: element))
      }

      capacity = buffer.count
      count = buffer.count
      nextFreeOffset = UInt32(truncatingIfNeeded: buffer.count)
      head = 0
      tail = UInt32(truncatingIfNeeded: buffer.count) - 1
    }

    deinit {
      baseAddress?.deinitialize(count: capacity)
      baseAddress?.deallocate()
    }

    /// Allocates space for a new link, growing the internal buffer if necessary.
    func allocateLink() -> UInt32 {
      if count == capacity {
        reserve(capacity: Swift.max(capacity * 2, 1))
      }

      let offset = UInt32(truncatingIfNeeded: nextFreeOffset)
      let nextNextFreeOffset = baseAddress![Int(nextFreeOffset)].next
      nextFreeOffset = nextNextFreeOffset != NONE
        ? nextNextFreeOffset
        : nextFreeOffset + 1

      return offset
    }

    func append(_ newElement: Element) {
      let offset = allocateLink()
      if head == NONE {
        head = offset
        baseAddress![Int(offset)] = Link(next: NONE, prev: NONE, element: newElement)
      } else {
        baseAddress![Int(tail)].next = offset
        baseAddress![Int(offset)] = Link(next: NONE, prev: tail, element: newElement)
      }
      tail = offset
      count += 1
    }

    func prepend(_ newElement: Element) {
      let offset = allocateLink()
      if head == NONE {
        head = offset
        baseAddress![Int(offset)] = Link(next: NONE, prev: NONE, element: newElement)
      } else {
        baseAddress![Int(head)].prev = offset
        baseAddress![Int(offset)] = Link(next: head, prev: NONE, element: newElement)
      }
      head = offset
      count += 1
    }

    func insert(_ newElement: Element, before offset: UInt32) {
      guard offset != NONE else {
        append(newElement)
        return
      }

      let prev = baseAddress![Int(offset)].prev
      let newOffset = allocateLink()
      if prev == NONE {
        head = newOffset
        baseAddress![Int(offset)].prev = newOffset
        baseAddress![Int(newOffset)] = Link(next: offset, prev: NONE, element: newElement)
      } else {
        baseAddress![Int(prev)].next = newOffset
        baseAddress![Int(offset)].prev = newOffset
        baseAddress![Int(newOffset)] = Link(next: offset, prev: prev, element: newElement)
      }
      count += 1
    }

    func remove(at offset: UInt32) {
      let next = baseAddress![Int(offset)].next
      let prev = baseAddress![Int(offset)].prev

      if next == NONE {
        tail = prev
      } else {
        baseAddress![Int(next)].prev = prev
      }

      if prev == NONE {
        head = next
      } else {
        baseAddress![Int(prev)].next = next
      }

      baseAddress![Int(offset)].next = nextFreeOffset
      baseAddress![Int(offset)].element = nil
      nextFreeOffset = offset
      count -= 1
    }

    func reverse() {
      var offset = head
      while offset != NONE {
        swap(&baseAddress![Int(offset)].next, &baseAddress![Int(offset)].prev)
        offset = baseAddress![Int(offset)].prev
      }
      swap(&head, &tail)
    }

    func reserve(capacity: Int) {
      if capacity <= self.capacity { return }

      // Copy the contents of the current buffer.
      let buffer = UnsafeMutablePointer<Link>.allocate(capacity: capacity)
      if let base = baseAddress {
        buffer.moveInitialize(from: base, count: self.capacity)
        base.deallocate()
      }

      // Initialize newly allocated space.
      let remainder = capacity - self.capacity
      buffer
        .advanced(by: self.capacity)
        .initialize(repeating: Link(next: NONE, prev: NONE, element: nil), count: remainder)

      baseAddress = buffer
      self.capacity = capacity
    }

    func copy() -> List {
      let clone = List()
      guard let base = baseAddress else { return clone }

      clone.baseAddress = .allocate(capacity: capacity)
      clone.baseAddress!.initialize(from: base, count: capacity)
      clone.capacity = capacity
      clone.count = count
      clone.nextFreeOffset = nextFreeOffset
      clone.head = head
      clone.tail = tail

      return clone
    }

  }

  private var list: List

  /// Creates a new, empty list.
  public init() {
    list = List()
  }

  /// Creates a new list containing the elements of the given sequence.
  ///
  /// - Warning: Indices of another list will not be valid in the new list.
  ///
  /// - Parameter elements: A sequence of elements.
  public init<S>(_ elements: S) where S: Sequence, S.Element == Element {
    if let l = elements.withContiguousStorageIfAvailable(List.init(_:)) {
      list = l
    } else {
      list = Array(elements).withUnsafeBufferPointer(List.init(_:))
    }
  }

  /// A Boolean value indicating whether the collection is empty.
  public var isEmpty: Bool { list.count == 0 }

  /// The number of elements in the list.
  public var count: Int { list.count }

  /// The total number of elements that the list can contain without allocating new storage.
  public var capacity: Int { list.capacity }

  /// The index of the first element in the list, assuming it is not empty.
  public var startIndex: Index { Index(offset: list.head) }

  /// The index of the last element in the list, assuming it is not empty.
  public var lastIndex: Index { Index(offset: list.tail) }

  /// The "past the end" index of the list -- that is, the index after the last valid index.
  public var endIndex: Index { Index(offset: NONE) }

  /// The next stable index of the list, i.e., the index that will be associated with the next
  /// element inserted into the list.
  public var nextStableIndex: Index { Index(offset: list.nextFreeOffset) }

  /// A collection containing the indices of this list, in order.
  public var indices: [Index] {
    var result: [Index] = []
    result.reserveCapacity(count)

    var i = startIndex
    while i != endIndex {
      result.append(i)
      i = index(after: i)
    }
    return result
  }

  /// Returns `true` if the index `i` precedes the index `j`.
  ///
  /// - Complexity: O(n), where n is the number of elements in the list.
  ///
  /// - Parameters:
  ///   - i: An index.
  ///   - j: Another index.
  public func isPreceeding(_ i: Index, _ j: Index) -> Bool {
    if i == j { return false }
    if j == endIndex { return true }

    var i = i
    while i != endIndex {
      if i == j { return true }
      i = index(after: i)
    }
    return false
  }

  /// Returns the immediate successor of the given index.
  ///
  /// - Parameter i: A valid index of the list. `i` must be a predecessor of `endIndex`.
  public func index(after i: Index) -> Index {
    precondition(i != endIndex)
    return Index(offset: list.baseAddress![Int(i.offset)].next)
  }

  /// Returns the immediate predecessor the given index.
  ///
  /// - Parameter i: A valid index of the list. `i` must be a successor of `startIndex`.
  public func index(before i: Index) -> Index {
    precondition(i != startIndex)
    return Index(offset: list.baseAddress![Int(i.offset)].prev)
  }

  /// Returns an index that is the specified distance from the given index.
  ///
  /// - Complexity: O(k), where k is the absolute value of `distance`.
  ///
  /// - Parameters:
  ///   - i: A valid index of the list. `i` must be a predecessor of `endIndex`.
  ///   - distance: The distance to offset `i`. `distance` must not offset `i` beyond the bounds of
  ///     the list.
  public func index(_ i: Index, offsetBy distance: Int) -> Index {
    var i = i
    if distance > 0 {
      for _ in 0 ..< distance { i = index(after: i) }
    } else {
      for _ in distance ..< 0 { i = index(before: i) }
    }
    return i
  }

  /// Returns the first index at which an element of the list satisfies the given predicate.
  ///
  /// - Parameter predicate: A predicate on the elements of the list.
  public func firstIndex(where predicate: (Element) throws -> Bool) rethrows -> Index? {
    var i = startIndex
    while i != endIndex {
      if try predicate(self[i]) { return i }
      i = index(after: i)
    }
    return nil
  }

  /// Returns the last index at which an element of the list satisfies the given predicate.
  ///
  /// - Parameter predicate: A predicate on the elements of the list.
  public func lastIndex(where predicate: (Element) throws -> Bool) rethrows -> Index? {
    var i = lastIndex
    if lastIndex == endIndex { return nil }
    while true {
      if try predicate(self[i]) {
        return i
      } else if i == startIndex {
        return nil
      }
      i = index(before: i)
    }
  }

  /// Adds a new element at the end of the list.
  ///
  /// - Complexity: O(1) on average, over many calls to `append(_:)` on the same list.
  ///
  /// - Parameter newElement: The element to append.
  public mutating func append(_ newElement: Element) {
    if !isKnownUniquelyReferenced(&list) {
      list = list.copy()
    }
    list.append(newElement)
  }

  /// Adds a new element at the beginning of the list.
  ///
  /// - Complexity: O(1) on average, over many calls to `append(_:)` on the same list.
  ///
  /// - Parameter newElement: The element to prepend.
  public mutating func prepend(_ newElement: Element) {
    if !isKnownUniquelyReferenced(&list) {
      list = list.copy()
    }
    list.prepend(newElement)
  }

  /// Inserts a new element before the specified index.
  ///
  /// The new element is inserted before the element currently at the specified index.
  ///
  /// - Complexity: O(1) on average, over many calls to `insert(_:before:)` on the same list.
  ///
  /// - Parameters:
  ///   - newElement: The element to insert.
  ///   - index: The index at which to insert the new element. `index` must be a valid index of the
  ///     list or equal to its `endIndex` property.
  public mutating func insert(_ newElement: Element, before index: Index) {
    if !isKnownUniquelyReferenced(&list) {
      list = list.copy()
    }
    list.insert(newElement, before: index.offset)
  }

  /// Removes the element at the given index.
  ///
  /// - Complexity: O(1)
  ///
  /// - Parameter index: The index of the element to remove.
  /// - Returns: The element that was removed.
  @discardableResult public mutating func remove(at index: Index) -> Element {
    if !isKnownUniquelyReferenced(&list) {
      list = list.copy()
    }
    let element = self[index]
    list.remove(at: index.offset)
    return element
  }

  public subscript(index: Index) -> Element {
    get {
      return list.baseAddress![Int(index.offset)].element!
    }
    set {
      if !isKnownUniquelyReferenced(&list) {
        list = list.copy()
      }
      list.baseAddress![Int(index.offset)].element = newValue
    }
    _modify {
      if !isKnownUniquelyReferenced(&list) {
        list = list.copy()
      }
      yield &list.baseAddress![Int(index.offset)].element!
    }
  }

  /// Exchanges the values at the specified indices of the list.
  ///
  /// - Complexity: O(1)
  ///
  /// - Parameters:
  ///   - i: The index of the first value to swap.
  ///   - j: The index of the second value to swap.
  public mutating func swapAt(_ i: Index, _ j: Index) {
    swap(
      &list.baseAddress![Int(i.offset)].element!,
      &list.baseAddress![Int(j.offset)].element!)
  }

  /// Reverses the elements of the collection in place.
  ///
  /// - Complexity: O(n), where n is the number of elements in the list.
  public mutating func reverse() {
    if isEmpty { return }
    if !isKnownUniquelyReferenced(&list) {
      list = list.copy()
    }
    list.reverse()
  }

  /// Returns a new list with all elements of this list in reverse order.
  ///
  /// - Complexity: O(n), where n is the number of elements in the list.
  public func reversed() -> StableDoublyLinkedList {
    var newList = self
    newList.reverse()
    return newList
  }

  /// Returns a sublist from the start of the list up to, but not including, the given index.
  ///
  /// - Complexity: O(1)
  ///
  /// - Parameter index: A valid index of the list.
  public func prefix(upTo end: Index) -> SubList {
    return SubList(startIndex: startIndex, endIndex: end, base: self)
  }

  /// Returns a sublist from the specified index to the end of the list.
  public func suffix(from start: Index) -> SubList {
    return SubList(startIndex: start, endIndex: endIndex, base: self)
  }

  /// Splits the list at the given index into two separate lists.
  ///
  /// - Complexity: O(n), where n is the number of elements in the list.
  ///
  /// - Parameter index: A valid index of the list.
  /// - Returns: A pair of lists `(first, second)`, `first` has the same contents of this list up
  ///   to, but not including, `index`; `second` contains the remainder.
  public func split(
    at index: Index
  ) -> (first: StableDoublyLinkedList, second: StableDoublyLinkedList) {
    if index == endIndex { return (first: self, second: StableDoublyLinkedList()) }
    return (
      first: StableDoublyLinkedList(prefix(upTo: index)),
      second: StableDoublyLinkedList(suffix(from: index)))
  }

  /// Reserves enough space to store the specified number of elements.
  ///
  /// - Parameter capacity: The requested number of elements to store.
  public mutating func reserve(capacity: Int) {
    if !isKnownUniquelyReferenced(&list) {
      list = list.copy()
    }
    list.reserve(capacity: capacity)
  }

}

extension StableDoublyLinkedList: Sequence {

  public struct Iterator: IteratorProtocol {

    fileprivate let base: StableDoublyLinkedList

    fileprivate var index: Index

    fileprivate let endIndex: Index

    public mutating func next() -> Element? {
      if index != endIndex {
        defer { index = base.index(after: index) }
        return base[index]
      } else {
        return nil
      }
    }

  }

  public var underestimatedCount: Int { count }

  public func makeIterator() -> Iterator {
    return Iterator(base: self, index: startIndex, endIndex: endIndex)
  }

}

extension StableDoublyLinkedList.SubList: Sequence {

  public typealias Iterator = StableDoublyLinkedList.Iterator

  public func makeIterator() -> Iterator {
    return Iterator(base: base, index: startIndex, endIndex: endIndex)
  }

}

extension StableDoublyLinkedList: Equatable where Element: Equatable {

  public static func == (lhs: StableDoublyLinkedList, rhs: StableDoublyLinkedList) -> Bool {
    if lhs.list === rhs.list { return true }

    var i = lhs.makeIterator()
    var j = rhs.makeIterator()
    while let a = i.next() {
      guard let b = j.next(), a == b else { return false }
    }
    return j.next() == nil
  }

}

extension StableDoublyLinkedList: Hashable where Element: Hashable {

  public func hash(into hasher: inout Hasher) {
    var i = makeIterator()
    while let a = i.next() {
      hasher.combine(a)
    }
  }

}

extension StableDoublyLinkedList: ExpressibleByArrayLiteral {

  public typealias ArrayLiteralElement = Element

  public init(arrayLiteral elements: Element...) {
    self.init(elements)
  }

}

extension StableDoublyLinkedList: CustomStringConvertible {

  public var description: String {
    return "[" + map(String.init(describing:)).joined(separator: ", ") + "]"
  }

}

extension StableDoublyLinkedList: CustomReflectable {

  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: Array(self), displayStyle: .collection)
  }

}
