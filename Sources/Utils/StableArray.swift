/// An array which guarantees stable indices.
///
/// - Note: `StableArray` does not satisfy the requirements of `Collection`, as `StableArray.Index`
///   does not conform to `Comparable`. However, you may use `StableArray.elements` to present the
///   elements of a stable array in a collection.
public struct StableArray<Element> {

  // The internal representation is a doubly linked list in which forward and backward pointers are
  // encoded as offset from the collection's raw storage.
  //
  // The raw storage grows automatically when inserting an element would exceed its capability, but
  // it never shinks. A linked list keeps track of the free buckets in the buffer to reuse memory
  // after an element has been removed.

  /// The header of the internal storage of a stable array.
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

  /// A bucket in the internal storage of a stable array.
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

  /// A linked list representing the internal storage of a stable array.
  fileprivate final class List: ManagedBuffer<Header, Bucket> {

    deinit {
      _ = withUnsafeMutablePointers({ (header, elements) in
        elements.deinitialize(count: header.pointee.capacity)
      })
    }

  }

  /// The internal storage of the array.
  fileprivate var storage: ManagedBuffer<Header, Bucket>?

  /// Creates an empty array.
  public init() {}

  /// Inserts `newElement` at the end of the array and returns its index.
  @discardableResult
  public mutating func append(_ newElement: Element) -> Index {
    if let tail = storage?.header.tailOffset {
      // Insert the new element after the tail element.
      return insert(newElement, after: Index(tail))
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

      return Index(0)
    }
  }

  /// Inserts `newElement` at the start of the array and returns its index.
  @discardableResult
  public mutating func prepend(_ newElement: Element) -> Index {
    if let head = storage?.header.headOffset {
      // Insert the new element before the head element.
      return insert(newElement, before: Index(head))
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

      return Index(0)
    }
  }

  /// Inserts `newElement` before the element at the specified position and returns its index.
  ///
  /// - Requires: `position` must be the index of an element in `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, before position: Index) -> Index {
    guard storage != nil,
          (position.rawValue >= 0) && (position.rawValue < storage!.header.capacity)
    else { preconditionFailure("index out of bound") }

    // Allocate new storage if `self` is full. Otherwise, make sure mutation is not shared.
    if storage!.header.count == storage!.header.capacity {
      reserveCapacity(storage!.header.capacity * 2)
    } else {
      uniquify()
    }

    // Insert the new element.
    return storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[position.rawValue].element != nil, "index out of bound")

      let newPosition = header.pointee.freeOffset
      let next = elements[newPosition].nextOffset

      elements[newPosition].element = newElement
      elements[newPosition].previousOffset = elements[position.rawValue].previousOffset
      elements[newPosition].nextOffset = position.rawValue

      header.pointee.count += 1
      header.pointee.freeOffset = next != -1 ? next : header.pointee.count

      if position.rawValue == header.pointee.headOffset {
        header.pointee.headOffset = newPosition
      } else {
        elements[elements[position.rawValue].previousOffset].nextOffset = newPosition
      }
      elements[position.rawValue].previousOffset = newPosition

      return Index(newPosition)
    })
  }

  /// Inserts `newElement` after the element at the specified position.
  ///
  /// - Requires: `position` must be the index of an element in `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, after position: Index) -> Index {
    guard storage != nil,
          (position.rawValue >= 0) && (position.rawValue < storage!.header.capacity)
    else { preconditionFailure("index out of bound") }

    // Allocate new storage if `self` is full. Otherwise, make sure mutation is not shared.
    if storage!.header.count == storage!.header.capacity {
      reserveCapacity(storage!.header.capacity * 2)
    } else {
      uniquify()
    }

    // Insert the new element.
    return storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[position.rawValue].element != nil, "index out of bound")

      let newPosition = header.pointee.freeOffset
      let next = elements[newPosition].nextOffset

      elements[newPosition].element = newElement
      elements[newPosition].nextOffset = elements[position.rawValue].nextOffset
      elements[newPosition].previousOffset = position.rawValue

      header.pointee.count += 1
      header.pointee.freeOffset = next != -1 ? next : header.pointee.count

      if position.rawValue == header.pointee.tailOffset {
        header.pointee.tailOffset = newPosition
      } else {
        elements[elements[position.rawValue].nextOffset].previousOffset = newPosition
      }
      elements[position.rawValue].nextOffset = newPosition

      return Index(newPosition)
    })
  }

  /// Removes the element at the specified index.
  @discardableResult
  public mutating func remove(at position: Index) -> Element {
    guard storage != nil,
          (position.rawValue >= 0) && (position.rawValue < storage!.header.capacity)
    else { preconditionFailure("index out of bound") }

    // Make sure mutation is not shared.
    uniquify()

    // Remove the element.
    let removed: Element = storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[position.rawValue].element != nil, "index out of bound")

      elements[elements[position.rawValue].previousOffset].nextOffset =
        elements[position.rawValue].nextOffset
      elements[position.rawValue].nextOffset = header.pointee.freeOffset
      header.pointee.count -= 1
      header.pointee.freeOffset = position.rawValue
      return elements[position.rawValue].element.release()
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

extension StableArray {

  /// A position in a stable array.
  public struct Index: Hashable {

    fileprivate var rawValue: Int

    fileprivate init(_ rawValue: Int) {
      self.rawValue = rawValue
    }

  }

  /// Indicates whether the array is empty.
  public var isEmpty: Bool {
    storage == nil
  }

  /// The number of elements in the array.
  public var count: Int {
    storage?.header.count ?? 0
  }

  /// The number of elements that can be stored in the array without allocating new storage.
  public var capacity: Int {
    storage?.header.capacity ?? 0
  }

  /// The indices of this array in order.
  public var indices: [Index] {
    guard let storage = storage else { return [] }
    return storage.withUnsafeMutablePointers({ (header, elements) in
      var result = [Index(header.pointee.headOffset)]
      while result.last!.rawValue != header.pointee.tailOffset {
        result.append(Index(elements[result.last!.rawValue].nextOffset))
      }
      return result
    })
  }

  /// The position of the first element in a non-empty array.
  public var startIndex: Index {
    Index(storage?.header.headOffset ?? 0)
  }

  /// The array's "past-the-end" position.
  public var endIndex: Index {
    guard let storage = storage else { return Index(0) }
    let offset = storage.header.tailOffset
    return storage.withUnsafeMutablePointerToElements({ elements in
      Index(elements[offset].nextOffset)
    })
  }

  /// The index that immediately follows `position`.
  public func index(after position: Index) -> Index {
    guard let storage = storage,
          (position.rawValue >= 0) && (position.rawValue < storage.header.capacity)
    else { preconditionFailure("index out of bound") }

    return storage.withUnsafeMutablePointerToElements({ elements in
      Index(elements[position.rawValue].nextOffset)
    })
  }

  /// The index that immediately precedes `position`.
  ///
  /// - Requires: `position` must be a valid index in the array and different than `startIndex`.
  public func index(before position: Index) -> Index {
    guard let storage = storage,
          (position != startIndex),
          (position.rawValue >= 0) && (position.rawValue < storage.header.capacity)
    else { preconditionFailure("index out of bound") }

    return storage.withUnsafeMutablePointerToElements({ elements in
      Index(elements[position.rawValue].previousOffset)
    })
  }

  /// Accesses the element at `position`.
  public subscript(position: Index) -> Element {
    get {
      guard let storage = storage,
            (position.rawValue >= 0) && (position.rawValue < storage.header.capacity)
      else { preconditionFailure("index out of bound") }
      return storage.withUnsafeMutablePointerToElements({ elements in
        elements[position.rawValue].element!
      })
    }
    _modify {
      guard let storage = storage,
            (position.rawValue >= 0) && (position.rawValue < storage.header.capacity)
      else { preconditionFailure("index out of bound") }

      var value = storage.withUnsafeMutablePointerToElements({ elements in
        elements[position.rawValue].element.release()
      })
      defer {
        storage.withUnsafeMutablePointerToElements({ elements in
          elements[position.rawValue].element = value
        })
      }
      yield &value
    }
  }

  /// The first element of the array.
  public var first: Element? {
    storage?.withUnsafeMutablePointers({ (header, elements) in
      elements[header.pointee.headOffset].element!
    })
  }

  /// The last element of the array.
  public var last: Element? {
    storage?.withUnsafeMutablePointers({ (header, elements) in
      elements[header.pointee.tailOffset].element!
    })
  }

  /// Returns the first position in which an element satisfies the given predicate.
  public func firstIndex(where predicate: (Element) throws -> Bool) rethrows -> Index? {
    var i = startIndex
    while i != endIndex {
      if try predicate(self[i]) { return i }
      i = index(after: i)
    }
    return nil
  }

  /// Returns the last position in which an element satisfies the given predicate.
  public func lastIndex(where predicate: (Element) throws -> Bool) rethrows -> Index? {
    guard var i = (storage?.header.tailOffset).map(Index.init) else { return nil }
    repeat {
      if try predicate(self[i]) { return i }
      i = index(before: i)
    } while i != startIndex
    return nil
  }

}

extension StableArray: Sequence {

  /// An iterator over a stable array.
  public struct Iterator: IteratorProtocol {

    fileprivate let base: StableArray

    fileprivate var index: StableArray.Index

    public mutating func next() -> Element? {
      if index != base.endIndex {
        defer { index = base.index(after: index) }
        return base[index]
      } else {
        return nil
      }
    }

  }

  public func makeIterator() -> Iterator {
    return Iterator(base: self, index: startIndex)
  }

}

extension StableArray: Equatable where Element: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    l.elementsEqual(r)
  }

}

extension StableArray: Hashable where Element: Hashable {

  public func hash(into hasher: inout Hasher) {
    for element in self {
      hasher.combine(element)
    }
  }

}

extension StableArray: ExpressibleByArrayLiteral {

  public init(arrayLiteral elements: Element...) {
    self.init()

    reserveCapacity(elements.capacity)
    for element in elements {
      append(element)
    }
  }

}

extension StableArray: CustomStringConvertible {

  public var description: String {
    String(describing: Array(self))
  }

}

extension StableArray: CustomReflectable {

  public var customMirror: Mirror {
    Mirror(self, unlabeledChildren: Array(self), displayStyle: .collection)
  }

}

extension StableArray {

  /// A collection that presents the elements of a stable array.
  public struct CollectionAdapter: BidirectionalCollection {

    public struct Index: Comparable, Hashable {

      /// The corresponding index of the underlying stable array.
      public let base: StableArray.Index

      fileprivate let offset: Int

      public func hash(into hasher: inout Hasher) {
        hasher.combine(offset)
      }

      public static func == (l: Self, r: Self) -> Bool { l.offset == r.offset }

      public static func < (l: Self, r: Self) -> Bool { l.offset < r.offset }

    }

    public typealias Element = StableArray.Element

    /// The underlying array.
    public var base: StableArray

    public var startIndex: Index {
      Index(base: base.startIndex, offset: 0)
    }

    public var endIndex: Index {
      Index(base: base.endIndex, offset: base.count)
    }

    public func index(after i: Index) -> Index {
      Index(base: base.index(after: i.base), offset: i.offset + 1)
    }

    public func index(before i: Index) -> Index {
      Index(base: base.index(before: i.base), offset: i.offset - 1)
    }

    public subscript(position: Index) -> StableArray<Element>.Element {
      base[position.base]
    }

  }

  /// A collection that presents the elements `self`.
  public var elements: CollectionAdapter { CollectionAdapter(base: self) }

}
