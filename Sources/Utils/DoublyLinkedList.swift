/// A doubly linked list.
public struct DoublyLinkedList<Element> {

  // The raw storage grows automatically when inserting an element would exceed its capability, but
  // it never shinks. A linked list keeps track of the free buckets in the buffer to reuse memory
  // after an element has been removed.

  /// The address of an element in a doubly linked list.
  public struct Address: Hashable {

    fileprivate var rawValue: Int

    fileprivate init(_ rawValue: Int) {
      self.rawValue = rawValue
    }

  }

  /// The header of the internal storage of a doubly linked list.
  fileprivate struct Header {

    /// The number of elements in the list.
    var count: Int

    /// The capacity of the list buffer.
    var capacity: Int

    /// The offset of the list head.
    var headOffset: Int

    /// The offset of the list tail.
    var tailOffset: Int

    /// The position of the next free bucket in the list buffer.
    var freeOffset: Int

  }

  /// A bucket in the internal storage of a doubly linked list.
  ///
  /// - Note: A bucket is said to be used if its element is not `nil`.
  fileprivate struct Bucket {

    /// If the bucket is used, represents the offset of the preceeding bucket in the list, or `-1`
    /// if such a bucket is not defined. Unspecified otherwise.
    var previousOffset: Int

    /// If the bucket is used, represents the offset of the succeeding bucket in the list, or `-1`
    /// if such a bucket is not defined. If the bucket is not used, represents the offset of the
    /// next free bucket.
    var nextOffset: Int

    /// If the bucket is used, the stored element. Otherwise `nil`.
    var element: Element?

  }

  /// A buffer representing the storage of a doubly linked list.
  fileprivate final class List: ManagedBuffer<Header, Bucket> {

    deinit {
      _ = withUnsafeMutablePointers({ (header, elements) in
        elements.deinitialize(count: header.pointee.capacity)
      })
    }

  }

  /// The internal storage of the list.
  fileprivate var storage: ManagedBuffer<Header, Bucket>?

  /// Creates an empty list.
  public init() {}

  /// The number of elements that can be stored in the list without allocating new storage.
  public var capacity: Int {
    storage?.header.capacity ?? 0
  }

  /// The address of the first element.
  public var firstAddress: Address? {
    (storage?.header.headOffset).map(Address.init(_:))
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
    (storage?.header.tailOffset).map(Address.init(_:))
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
    guard let storage = storage,
          (address.rawValue >= 0) && (address.rawValue < storage.header.capacity)
    else { preconditionFailure("address out of bound") }

    guard address != lastAddress else { return nil }
    return storage.withUnsafeMutablePointerToElements({ elements in
      Address(elements[address.rawValue].nextOffset)
    })
  }

  /// The address of the element that immediately precedes the element at `address`.
  public func address(before address: Address) -> Address? {
    guard let storage = storage,
          (address.rawValue >= 0) && (address.rawValue < storage.header.capacity)
    else { preconditionFailure("address out of bound") }

    guard address != firstAddress else { return nil }
    return storage.withUnsafeMutablePointerToElements({ elements in
      Address(elements[address.rawValue].previousOffset)
    })
  }

  /// Accesses the element at `address`.
  public subscript(address: Address) -> Element {
    get {
      guard let storage = storage,
            (address.rawValue >= 0) && (address.rawValue < storage.header.capacity)
      else { preconditionFailure("address out of bound") }
      return storage.withUnsafeMutablePointerToElements({ elements in
        elements[address.rawValue].element!
      })
    }
    _modify {
      guard let storage = storage,
            (address.rawValue >= 0) && (address.rawValue < storage.header.capacity)
      else { preconditionFailure("address out of bound") }

      var value = storage.withUnsafeMutablePointerToElements({ elements in
        elements[address.rawValue].element.release()
      })
      defer {
        storage.withUnsafeMutablePointerToElements({ elements in
          elements[address.rawValue].element = value
        })
      }
      yield &value
    }
  }

  /// Inserts `newElement` at the end of the list and returns its address.
  @discardableResult
  public mutating func append(_ newElement: Element) -> Address {
    if let tail = storage?.header.tailOffset {
      // Insert the new element after the tail element.
      return insert(newElement, after: Address(tail))
    } else {
      // Allocate new storage if the `self` is empty.
      storage = List.create(
        minimumCapacity: 1,
        makingHeaderWith: { _ in
          Header(count: 1, capacity: 1, headOffset: 0, tailOffset: 0, freeOffset: 1)
        })
      storage!.withUnsafeMutablePointerToElements({ elements in
        elements.initialize(to: Bucket(previousOffset: -1, nextOffset: -1, element: newElement))
      })

      return Address(0)
    }
  }

  /// Inserts `newElement` at the start of the list and returns its address.
  @discardableResult
  public mutating func prepend(_ newElement: Element) -> Address {
    if let head = storage?.header.headOffset {
      // Insert the new element before the head element.
      return insert(newElement, before: Address(head))
    } else {
      // Allocate new storage if the `self` is empty.
      storage = List.create(
        minimumCapacity: 1,
        makingHeaderWith: { _ in
          Header(count: 1, capacity: 1, headOffset: 0, tailOffset: 0, freeOffset: 1)
        })
      storage!.withUnsafeMutablePointerToElements({ elements in
        elements.initialize(to: Bucket(previousOffset: -1, nextOffset: -1, element: newElement))
      })

      return Address(0)
    }
  }

  /// Inserts `newElement` before the element at `address` and returns its address.
  ///
  /// - Requires: `address` must be the address of an element in `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, before address: Address) -> Address {
    guard storage != nil,
          (address.rawValue >= 0) && (address.rawValue < storage!.header.capacity)
    else { preconditionFailure("address out of bound") }

    // Allocate new storage if `self` is full. Otherwise, make sure mutation is not shared.
    if storage!.header.count == storage!.header.capacity {
      reserveCapacity(storage!.header.capacity * 2)
    } else {
      uniquify()
    }

    // Insert the new element.
    return storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[address.rawValue].element != nil, "address out of bound")

      let newAddress = header.pointee.freeOffset
      let next = elements[newAddress].nextOffset

      elements[newAddress].element = newElement
      elements[newAddress].previousOffset = elements[address.rawValue].previousOffset
      elements[newAddress].nextOffset = address.rawValue

      header.pointee.count += 1
      header.pointee.freeOffset = next != -1 ? next : header.pointee.count

      if address.rawValue == header.pointee.headOffset {
        header.pointee.headOffset = newAddress
      } else {
        elements[elements[address.rawValue].previousOffset].nextOffset = newAddress
      }
      elements[address.rawValue].previousOffset = newAddress

      return Address(newAddress)
    })
  }

  /// Inserts `newElement` after the element at `address` and returns its address.
  ///
  /// - Requires: `address` must be a valid an address in `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, after address: Address) -> Address {
    guard storage != nil,
          (address.rawValue >= 0) && (address.rawValue < storage!.header.capacity)
    else { preconditionFailure("address out of bound") }

    // Allocate new storage if `self` is full. Otherwise, make sure mutation is not shared.
    if storage!.header.count == storage!.header.capacity {
      reserveCapacity(storage!.header.capacity * 2)
    } else {
      uniquify()
    }

    // Insert the new element.
    return storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[address.rawValue].element != nil, "address out of bound")

      let newAddress = header.pointee.freeOffset
      let next = elements[newAddress].nextOffset

      elements[newAddress].element = newElement
      elements[newAddress].nextOffset = elements[address.rawValue].nextOffset
      elements[newAddress].previousOffset = address.rawValue

      header.pointee.count += 1
      header.pointee.freeOffset = next != -1 ? next : header.pointee.count

      if address.rawValue == header.pointee.tailOffset {
        header.pointee.tailOffset = newAddress
      } else {
        elements[elements[address.rawValue].nextOffset].previousOffset = newAddress
      }
      elements[address.rawValue].nextOffset = newAddress

      return Address(newAddress)
    })
  }

  /// Removes the element at `address`.
  @discardableResult
  public mutating func remove(at address: Address) -> Element {
    guard storage != nil,
          (address.rawValue >= 0) && (address.rawValue < storage!.header.capacity)
    else { preconditionFailure("address out of bound") }

    // Make sure mutation is not shared.
    uniquify()

    // Remove the element.
    let removed: Element = storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[address.rawValue].element != nil, "address out of bound")

      elements[elements[address.rawValue].previousOffset].nextOffset =
        elements[address.rawValue].nextOffset
      elements[address.rawValue].nextOffset = header.pointee.freeOffset
      header.pointee.count -= 1
      header.pointee.freeOffset = address.rawValue
      return elements[address.rawValue].element.release()
    })

    if storage!.header.count == 0 {
      storage = nil
    }

    return removed
  }

  /// Reserves enough space to store the `minimumCapacity` elements without allocating new storage.
  public mutating func reserveCapacity(_ minimumCapacity: Int) {
    let oldCapacity = storage?.header.capacity ?? 0
    let newCapacity = minimumCapacity.roundedUpToNextPowerOfTwo
    if oldCapacity >= newCapacity { return }

    let newStorage = List.create(
      minimumCapacity: newCapacity,
      makingHeaderWith: { _ in storage!.header })

    newStorage.withUnsafeMutablePointers({ (newHeader, destination) in
      newHeader.pointee.capacity = newCapacity
      storage!.withUnsafeMutablePointerToElements({ source in
        destination.moveInitialize(from: source, count: oldCapacity)
        destination.advanced(by: oldCapacity)
          .initialize(
            repeating: Bucket(previousOffset: -1, nextOffset: -1, element: nil),
            count: newCapacity - oldCapacity)
      })
    })

    // Set capacity to 0 so that the destructor doesn't deinitialize moved elements.
    storage!.header.capacity = 0
    storage = newStorage
  }

  /// Copy the internal storage of `self` if it is shared.
  private mutating func uniquify() {
    if storage == nil || isKnownUniquelyReferenced(&storage) { return }

    // Storage is shared.
    let newStorage = List.create(
      minimumCapacity: storage!.capacity,
      makingHeaderWith: { _ in storage!.header })

    newStorage.withUnsafeMutablePointers({ (newHeader, destination) in
      storage!.withUnsafeMutablePointerToElements({ source in
        destination.moveInitialize(from: source, count: newHeader.pointee.capacity)
      })
    })

    // Set capacity to 0 so that the destructor doesn't deinitialize moved elements.
    storage!.header.capacity = 0
    storage = newStorage
  }

}

extension DoublyLinkedList: BidirectionalCollection, MutableCollection {

  public struct Index: Comparable, Hashable {

    /// The address corresponding to that index.
    public let address: Address

    fileprivate let offset: Int

    public func hash(into hasher: inout Hasher) {
      hasher.combine(offset)
    }

    public static func == (l: Self, r: Self) -> Bool { l.offset == r.offset }

    public static func < (l: Self, r: Self) -> Bool { l.offset < r.offset }

  }

  public var isEmpty: Bool {
    storage == nil
  }

  public var count: Int {
    storage?.header.count ?? 0
  }

  /// The first element of the list.
  public var first: Element? {
    storage?.withUnsafeMutablePointers({ (header, elements) in
      elements[header.pointee.headOffset].element!
    })
  }

  /// The last element of the list.
  public var last: Element? {
    storage?.withUnsafeMutablePointers({ (header, elements) in
      elements[header.pointee.tailOffset].element!
    })
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
    Index(address: address(before: i.address)!, offset: i.offset - 1)
  }

  public subscript(position: Index) -> Element {
    _read   { yield self[position.address] }
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

  public var description: String {
    String(describing: Array(self))
  }

}

extension DoublyLinkedList: CustomReflectable {

  public var customMirror: Mirror {
    Mirror(self, unlabeledChildren: Array(self), displayStyle: .collection)
  }

}
