/// A doubly linked list.
public struct DoublyLinkedList<Element> {

  // The raw storage grows automatically when inserting an element would exceed its capability, but
  // it never shinks. A linked list keeps track of the free buckets in the buffer to reuse memory
  // after an element has been removed.

  /// The address of an element in a doubly linked list.
  public struct Address: Hashable {

    public fileprivate(set) var rawValue: Int

    fileprivate init(_ rawValue: Int) {
      self.rawValue = rawValue
    }

    /// Returns whether the element stored at `self` precedes that stored at `other` in `list`.
    public func precedes(_ other: Address, in list: DoublyLinkedList) -> Bool {
      if self == other { return false }

      var current = self
      while let next = list.address(after: current) {
        if next == other { return true }
        current = next
      }
      return false
    }

  }

  /// A collection with the addresses of a doubly-linked list.
  public struct Addresses {

    private var base: DoublyLinkedList.Indices

    fileprivate init(_ base: DoublyLinkedList) {
      self.base = base.indices
    }

  }

  /// A bucket in the internal storage of a doubly linked list.
  ///
  /// - Note: A bucket is said to be used if its element is not `nil`.
  fileprivate struct Bucket {

    /// If the bucket is used, represents the offset of the preceding bucket in the list, or `-1`
    /// if such a bucket is not defined; unspecified otherwise.
    var previousOffset: Int

    /// If the bucket is used, the offset of the succeeding bucket in the list, or `-1` if such a
    /// bucket is not defined; If the bucket is not used, the offset of the next free bucket.
    var nextOffset: Int

    /// If the bucket is used, the stored element, or `nil` otherwise.
    var element: Element?

  }

  /// The number of elements in the list.
  public private(set) var count: Int = 0

  /// The offset of the list head.
  private var headOffset: Int = -1

  /// The offset of the list tail.
  private var tailOffset: Int = -1

  /// The position of the next free bucket in the list buffer.
  private var freeOffset: Int = 0

  /// The elements in list.
  fileprivate var storage: [Bucket] = []

  /// Creates an empty list.
  public init() {}

  /// The number of elements that can be stored in the list without allocating new storage.
  public var capacity: Int { storage.capacity }

  /// The address of the first element.
  public var firstAddress: Address? {
    storage.isEmpty ? nil : Address(headOffset)
  }

  /// Returns the first address at which an element satisfies the given predicate.
  public func firstAddress(where predicate: (Element) throws -> Bool) rethrows -> Address? {
    guard var i = firstAddress else { return nil }
    while true {
      if try predicate(self[i]) { return i }
      guard let next = address(after: i) else { return nil }
      i = next
    }
  }

  /// The address of the last element.
  public var lastAddress: Address? {
    storage.isEmpty ? nil : Address(tailOffset)
  }

  /// Returns the last address at which an element satisfies the given predicate.
  public func lastAddress(where predicate: (Element) throws -> Bool) rethrows -> Address? {
    guard var i = lastAddress else { return nil }
    while true {
      if try predicate(self[i]) { return i }
      guard let next = address(before: i) else { return nil }
      i = next
    }
  }

  /// The address of the element that immediately follows the element at `address`.
  public func address(after address: Address) -> Address? {
    precondition(isInBounds(address), "address out of bounds")
    if address == lastAddress { return nil }
    return Address(storage[address.rawValue].nextOffset)
  }

  /// The address of the element that immediately precedes the element at `address`.
  public func address(before address: Address) -> Address? {
    precondition(isInBounds(address), "address out of bounds")
    if address == firstAddress { return nil }
    return Address(storage[address.rawValue].previousOffset)
  }

  /// The addresses in the list.
  public var addresses: Addresses { Addresses(self) }

  /// Accesses the element at `address`.
  public subscript(address: Address) -> Element {
    get {
      precondition(isInBounds(address), "address out of bounds")
      return storage[address.rawValue].element!
    }
    _modify {
      precondition(isInBounds(address), "address out of bounds")
      yield &storage[address.rawValue].element!
    }
  }

  /// Inserts `newElement` at the end of the list and returns its address.
  @discardableResult
  public mutating func append(_ newElement: Element) -> Address {
    if storage.isEmpty {
      count = 1
      headOffset = 0
      tailOffset = 0
      freeOffset = 1
      storage = [Bucket(previousOffset: -1, nextOffset: -1, element: newElement)]
      return Address(0)
    } else if count == 0 {
      let newAddress = Address(freeOffset)
      count = 1
      headOffset = freeOffset
      tailOffset = freeOffset
      freeOffset = storage[newAddress.rawValue].nextOffset
      storage[newAddress.rawValue] = Bucket(
        previousOffset: -1, nextOffset: -1, element: newElement)
      return newAddress
    } else {
      return insert(newElement, after: Address(tailOffset))
    }
  }

  /// Inserts `newElement` at the start of the list and returns its address.
  @discardableResult
  public mutating func prepend(_ newElement: Element) -> Address {
    if storage.isEmpty {
      count = 1
      headOffset = 0
      tailOffset = 0
      freeOffset = 1
      storage = [Bucket(previousOffset: -1, nextOffset: -1, element: newElement)]
      return Address(0)
    } else if count == 0 {
      count = 1
      headOffset = 0
      tailOffset = 0
      freeOffset = 1
      storage = [Bucket(previousOffset: -1, nextOffset: -1, element: newElement)]
      return Address(0)
    } else {
      return insert(newElement, before: Address(headOffset))
    }
  }

  /// Inserts `newElement` after the element at `address` and returns its address.
  ///
  /// - Requires: `address` must be a valid an address in `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, after address: Address) -> Address {
    precondition(isInBounds(address), "address out of bounds")

    let newAddress: Address
    if freeOffset == storage.count {
      newAddress = Address(storage.count)
      freeOffset = storage.count + 1

      storage.append(
        Bucket(
          previousOffset: address.rawValue,
          nextOffset: storage[address.rawValue].nextOffset,
          element: newElement))
    } else {
      newAddress = Address(freeOffset)
      freeOffset = storage[freeOffset].nextOffset

      storage[newAddress.rawValue] = Bucket(
        previousOffset: address.rawValue,
        nextOffset: storage[address.rawValue].nextOffset,
        element: newElement)
    }

    if address.rawValue == tailOffset {
      tailOffset = newAddress.rawValue
    } else {
      storage[storage[address.rawValue].nextOffset].previousOffset = newAddress.rawValue
    }
    storage[address.rawValue].nextOffset = newAddress.rawValue

    count += 1
    return newAddress
  }

  /// Inserts `newElement` before the element at `address` and returns its address.
  ///
  /// - Requires: `address` must be the address of an element in `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, before address: Address) -> Address {
    precondition(isInBounds(address), "address out of bounds")

    let newAddress: Address
    if freeOffset == storage.count {
      newAddress = Address(storage.count)
      freeOffset = storage.count + 1

      storage.append(
        Bucket(
          previousOffset: storage[address.rawValue].previousOffset,
          nextOffset: address.rawValue,
          element: newElement))
    } else {
      newAddress = Address(freeOffset)
      freeOffset = storage[freeOffset].nextOffset

      storage[newAddress.rawValue] = Bucket(
        previousOffset: storage[address.rawValue].previousOffset,
        nextOffset: address.rawValue,
        element: newElement)
    }

    if address.rawValue == headOffset {
      headOffset = newAddress.rawValue
    } else {
      storage[storage[address.rawValue].previousOffset].nextOffset = newAddress.rawValue
    }
    storage[address.rawValue].previousOffset = newAddress.rawValue

    count += 1
    return newAddress
  }

  /// Inserts `newElement` at the given position and returns its address.
  ///
  /// The new element is inserted before the element currently at `position`. You can pass a
  /// "past the end" index to append `newElement` at the end of `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, at position: Index) -> Address {
    if position == endIndex {
      return append(newElement)
    } else {
      return insert(newElement, before: position.address)
    }
  }

  /// Removes the element at `address`.
  @discardableResult
  public mutating func remove(at address: Address) -> Element {
    precondition(isInBounds(address), "address out of bounds")

    let previous = storage[address.rawValue].previousOffset
    if previous != -1 {
      storage[previous].nextOffset = storage[address.rawValue].nextOffset
    }

    let next = storage[address.rawValue].nextOffset
    if next != -1 {
      storage[next].previousOffset = storage[address.rawValue].previousOffset
    }

    storage[address.rawValue].nextOffset = freeOffset

    count -= 1
    freeOffset = address.rawValue
    if address.rawValue == tailOffset {
      tailOffset = previous
    }

    return storage[address.rawValue].element.release()
  }

  /// Reserves enough space to store the `minimumCapacity` elements without allocating new storage.
  public mutating func reserveCapacity(_ minimumCapacity: Int) {
    storage.reserveCapacity(minimumCapacity)
  }

  /// Returns whether `address` is in bounds.
  private func isInBounds(_ address: Address) -> Bool {
    !storage.isEmpty && (address.rawValue >= 0) && (address.rawValue < storage.count)
  }

}

extension DoublyLinkedList: BidirectionalCollection, MutableCollection {

  public struct Index: Comparable, Hashable {

    /// The address corresponding to that index.
    public let address: Address

    public let offset: Int

    public func hash(into hasher: inout Hasher) {
      hasher.combine(offset)
    }

    public static func == (l: Self, r: Self) -> Bool { l.offset == r.offset }

    public static func < (l: Self, r: Self) -> Bool { l.offset < r.offset }

  }

  public var isEmpty: Bool { count == 0 }

  /// The first element of the list.
  public var first: Element? {
    storage.isEmpty ? nil : storage[headOffset].element!
  }

  /// The last element of the list.
  public var last: Element? {
    storage.isEmpty ? nil : storage[tailOffset].element!
  }

  public var startIndex: Index {
    Index(address: firstAddress ?? Address(0), offset: 0)
  }

  public var endIndex: Index {
    Index(address: Address(0), offset: count)
  }

  public func index(after i: Index) -> Index {
    Index(address: address(after: i.address) ?? Address(0), offset: i.offset + 1)
  }

  public func index(before i: Index) -> Index {
    i.offset == count
      ? Index(address: lastAddress!, offset: i.offset - 1)
      : Index(address: address(before: i.address)!, offset: i.offset - 1)
  }

  public subscript(position: Index) -> Element {
    _read { yield self[position.address] }
    _modify { yield &self[position.address] }
  }

}

extension DoublyLinkedList: Equatable where Element: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    l.elementsEqual(r)
  }

}

extension DoublyLinkedList: Hashable where Element: Hashable {

  public func hash(into hasher: inout Hasher) {
    for element in self {
      hasher.combine(element)
    }
  }

}

extension DoublyLinkedList: ExpressibleByArrayLiteral {

  public init(arrayLiteral elements: Element...) {
    self.init()

    reserveCapacity(elements.capacity)
    for element in elements {
      append(element)
    }
  }

}

extension DoublyLinkedList: CustomStringConvertible {

  public var description: String { "[\(list: self, joinedBy: ", ")]" }

}

extension DoublyLinkedList.Address: CustomStringConvertible {

  public var description: String { String(describing: rawValue) }

}

extension DoublyLinkedList.Addresses: BidirectionalCollection {

  public typealias Index = DoublyLinkedList.Indices.Index

  public typealias Element = DoublyLinkedList.Address

  public var startIndex: Index { base.startIndex }

  public var endIndex: Index { base.endIndex }

  public func index(after i: Index) -> Index {
    base.index(after: i)
  }

  public func index(before i: Index) -> Index {
    base.index(before: i)
  }

  public subscript(position: Index) -> DoublyLinkedList.Address {
    base[position].address
  }

}
