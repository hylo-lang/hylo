/// An array which guarantees stable indices.
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
  public mutating func append(_ newElement: Element) -> Int {
    if let tail = storage?.header.tailOffset {
      // Insert the new element after the tail element.
      return insert(newElement, after: tail)
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

      return 0
    }
  }

  /// Inserts `newElement` at the start of the array and returns its index.
  @discardableResult
  public mutating func prepend(_ newElement: Element) -> Int {
    if let head = storage?.header.headOffset {
      // Insert the new element before the head element.
      return insert(newElement, before: head)
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

      return 0
    }
  }

  /// Inserts `newElement` before the element at the specified position and returns its index.
  ///
  /// - Requires: `position` must be the index of an element in `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, before position: Int) -> Int {
    guard storage != nil,
          (position >= 0) && (position < storage!.header.capacity)
    else { preconditionFailure("index out of bound") }

    // Allocate new storage if `self` is full. Otherwise, make sure mutation is not shared.
    if storage!.header.count == storage!.header.capacity {
      reserveCapacity(storage!.header.capacity * 2)
    } else {
      uniquify()
    }

    // Insert the new element.
    return storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[position].element != nil, "index out of bound")

      let newPosition = header.pointee.freeOffset
      let next = elements[newPosition].nextOffset

      elements[newPosition].element = newElement
      elements[newPosition].previousOffset = elements[position].previousOffset
      elements[newPosition].nextOffset = position

      header.pointee.count += 1
      header.pointee.freeOffset = next != -1 ? next : header.pointee.count

      if position == header.pointee.headOffset {
        header.pointee.headOffset = newPosition
      } else {
        elements[elements[position].previousOffset].nextOffset = newPosition
      }
      elements[position].previousOffset = newPosition

      return newPosition
    })
  }

  /// Inserts `newElement` after the element at the specified position.
  ///
  /// - Requires: `position` must be the index of an element in `self`.
  @discardableResult
  public mutating func insert(_ newElement: Element, after position: Int) -> Int {
    guard storage != nil,
          (position >= 0) && (position < storage!.header.capacity)
    else { preconditionFailure("index out of bound") }

    // Allocate new storage if `self` is full. Otherwise, make sure mutation is not shared.
    if storage!.header.count == storage!.header.capacity {
      reserveCapacity(storage!.header.capacity * 2)
    } else {
      uniquify()
    }

    // Insert the new element.
    return storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[position].element != nil, "index out of bound")

      let newPosition = header.pointee.freeOffset
      let next = elements[newPosition].nextOffset

      elements[newPosition].element = newElement
      elements[newPosition].nextOffset = elements[position].nextOffset
      elements[newPosition].previousOffset = position

      header.pointee.count += 1
      header.pointee.freeOffset = next != -1 ? next : header.pointee.count

      if position == header.pointee.tailOffset {
        header.pointee.tailOffset = newPosition
      } else {
        elements[elements[position].nextOffset].previousOffset = newPosition
      }
      elements[position].nextOffset = newPosition

      return newPosition
    })
  }

  /// Removes the element at the specified index.
  @discardableResult
  public mutating func remove(at position: Int) -> Element {
    guard storage != nil,
          (position >= 0) && (position < storage!.header.capacity)
    else { preconditionFailure("index out of bound") }

    // Make sure mutation is not shared.
    uniquify()

    // Remove the element.
    let removed: Element = storage!.withUnsafeMutablePointers({ (header, elements) in
      precondition(elements[position].element != nil, "index out of bound")

      elements[elements[position].previousOffset].nextOffset = elements[position].nextOffset
      elements[position].nextOffset = header.pointee.freeOffset
      header.pointee.count -= 1
      header.pointee.freeOffset = position
      return elements[position].element.release()
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

  public typealias Index = Int

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
  public var indices: [Int] {
    guard let storage = storage else { return [] }
    return storage.withUnsafeMutablePointers({ (header, elements) in
      var result = [header.pointee.headOffset]
      while result.last! != header.pointee.tailOffset {
        result.append(elements[result.last!].nextOffset)
      }
      return result
    })
  }

  /// The position of the first element in a non-empty array.
  public var startIndex: Int {
    storage?.header.headOffset ?? 0
  }

  /// The array's "past-the-end" position.
  public var endIndex: Int {
    guard let storage = storage else { return 0 }
    let offset = storage.header.tailOffset
    return storage.withUnsafeMutablePointerToElements({ elements in
      elements[offset].nextOffset
    })
  }

  /// The index that immediately follows `position`.
  public func index(after position: Int) -> Int {
    guard let storage = storage,
          (position >= 0) && (position < storage.header.capacity)
    else { preconditionFailure("index out of bound") }

    return storage.withUnsafeMutablePointerToElements({ elements in
      elements[position].nextOffset
    })
  }

  /// Accesses the element at `position`.
  public subscript(position: Int) -> Element {
    get {
      guard let storage = storage,
            (position >= 0) && (position < storage.header.capacity)
      else { preconditionFailure("index out of bound") }
      return storage.withUnsafeMutablePointerToElements({ elements in
        elements[position].element!
      })
    }
    _modify {
      guard let storage = storage,
            (position >= 0) && (position < storage.header.capacity)
      else { preconditionFailure("index out of bound") }

      var value = storage.withUnsafeMutablePointerToElements({ elements in
        elements[position].element.release()
      })
      defer {
        storage.withUnsafeMutablePointerToElements({ elements in
          elements[position].element = value
        })
      }
      yield &value
    }
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
