/// The integer type that is used to represent slot indices.
///
/// The number of bitmaps is computed so that it each bucket contain exactly `SlotIndex.max`
/// elements. Hence, larger slot indices result in larger bucket sizes.
fileprivate typealias SlotIndex = UInt16

/// An unordered, random-access collection that guarantees stable indices and O(1) removal.
///
/// Use this data structure when you need stable indices (e.g., to store stable identities in other
/// data structures) while retaining the ability to mutate the collection in place.
///
/// A slab is implemented as a collection of fixed-size arrays with tombstones. Whenever an element
/// is removed, its slot is marked as a "tombstone" and nothing else happens. Hence, the indices of
/// other elements in the collection remain stable.
///
/// The collection is unordered, as tombstones are reused to insert new elements.
public struct Slab<T> {

  /// The index type for slabs.
  public struct Index {

    fileprivate let bucket: Int

    fileprivate let slot: SlotIndex

  }

  /// The internal storage of a slab allocator.
  private final class _Storage {

    /// The number of elements in the storage.
    var count: Int = 0

    /// The number of buckets in this storage.
    var bucketCount: Int = 0

    /// The base address of this storage's contents.
    private var base: UnsafeMutableRawPointer?

    /// Creates a new storage capable of storing at least the specified number of elements.
    init(minimumCapacity: Int) {
      bucketCount = (minimumCapacity + _Storage.bucketCapacity - 1) / _Storage.bucketCapacity
      if bucketCount > 0 {
        // Allocate the storage's memory.
        base = .allocate(
          byteCount: bucketCount * _Storage.bucketStride,
          alignment: _Storage.bucketAlignment)

        // Zero-initialize all bucket headers.
        for b in 0 ..< bucketCount {
          withBucket(b, { (header, _) in
            header.initialize(repeating: 0, count: _Storage.bitmapCount)
          })
        }
      }
    }

    /// Creates a new storage copying the contents of an existing one.
    convenience init(copying other: _Storage, minimumCapacity: Int = 0) {
      let actualCapacity = Swift.max(minimumCapacity, other.bucketCount * _Storage.bucketCapacity)
      self.init(minimumCapacity: actualCapacity)

      // Copy the other storage's contents.
      count = other.count
      for b in 0 ..< other.bucketCount {
        withBucket(b, { (thisHeader, thisBody)  in
          other.withBucket(b, { (thatHeader, thatBody) in
            // Copy the bucket's header.
            thisHeader.initialize(from: thatHeader, count: _Storage.bitmapCount)

            // Copy the allocated slots of the bucket's body.
            for i in 0 ..< _Storage.bitmapCount {
              let offset = i * UInt.bitWidth
              for j in 0 ..< UInt.bitWidth {
                if thatHeader[i] & (1 << j) != 0 {
                  thisBody.advanced(by: j + offset).initialize(to: thatBody[j + offset])
                }
              }
            }
          })
        })
      }
    }

    deinit {
      // Deinitialize the allocated slots.
      for b in 0 ..< bucketCount {
        withBucket(b, { (header, body) in
          for i in 0 ..< _Storage.bitmapCount {
            let offset = i * UInt.bitWidth
            for j in 0 ..< UInt.bitWidth {
              if header[i] & (1 << j) != 0 {
                body.advanced(by: j + offset).deinitialize(count: 1)
              }
            }
          }
        })
      }
      base?.deallocate()
    }

    /// Returns the index of the first free slot in the specified bucket.
    func firstSlot(in bucket: Int) -> Int? {
      return withBucket(bucket, { (header, _) in
        for i in 0 ..< _Storage.bitmapCount where header[i] != 0 {
          // Find the first non-zero least significant bit in the busy map.
          let busymap = header[i]
          let lsb = busymap & ~(busymap - 1)
          return Int(log2_64(lsb)) + i * UInt.bitWidth
        }
        return nil
      })
    }

    /// Inserts an element into the storage, allocating a new bucket if necessary.
    func insert(_ newElement: T) -> Index {
      defer { count += 1 }

      // Try to insert the new element in a free slot.
      for b in 0 ..< bucketCount {
        if let index = withBucket(b, { (header, body) -> Index? in
          for i in 0 ..< _Storage.bitmapCount where header[i] != .max {
            // Find the first non-zero least significant bit in the free map.
            let freemap = ~header[i]
            let lsb = freemap & ~(freemap - 1)

            // Insert the element.
            let s = Int(log2_64(lsb)) + i * UInt.bitWidth
            header[i] = header[i] | lsb
            body.advanced(by: s).initialize(to: newElement)
            return Index(bucket: b, slot: SlotIndex(truncatingIfNeeded: s))
          }

          // The bucket is full.
          return nil
        }) { return index }
      }

      // The storage is full; let's resize it.
      let newBase = UnsafeMutableRawPointer.allocate(
        byteCount: (bucketCount + 1) * _Storage.bucketStride,
        alignment: _Storage.bucketAlignment)
      if let base = base {
        newBase.copyMemory(from: base, byteCount: _Storage.bucketStride * bucketCount)
        base.deallocate()
      }
      bucketCount += 1
      base = newBase

      withBucket(bucketCount - 1, { (header, body) in
        header.initialize(repeating: 0, count: _Storage.bitmapCount)
        header[0] = 1
        body.initialize(to: newElement)
      })
      return Index(bucket: bucketCount - 1, slot: 0)
    }

    /// Removes an element from the storage.
    func remove(at position: Index) -> T {
      defer { count -= 1 }

      return withBucket(position.bucket, { (header, body) in
        let s = Int(position.slot)
        let i = s / UInt.bitWidth
        precondition(header[i] & (1 << (s % UInt.bitWidth)) != 0, "invalid index")

        // Remove the element.
        header[i] = header[i] & ~(1 << (s % UInt.bitWidth))
        return body.advanced(by: s).move()
      })
    }

    /// Calls the given closure with the bucket at the specified index.
    func withBucket<R>(
      _ index: Int,
      _ action: (_ header: UnsafeMutablePointer<UInt>, _ body: UnsafeMutablePointer<T>) -> R
    ) -> R {
      let bucket = base!.advanced(by: index * _Storage.bucketStride)
      let header = bucket.assumingMemoryBound(to: UInt.self)
      let body = bucket.advanced(by: _Storage.bucketBodyOffset).assumingMemoryBound(to: T.self)
      return action(header, body)
    }

    /// The memory alignment of a bucket.
    class var bucketAlignment: Int {
      return Swift.max(MemoryLayout<UInt>.alignment, MemoryLayout<T>.alignment)
    }

    /// The stride of a bucket.
    class var bucketStride: Int {
      let size = bucketBodyOffset + MemoryLayout<T>.stride * bucketCapacity
      return size.roundedAwayFromZero(toNearestMultipleOf: bucketAlignment)
    }

    /// The offset of a bucket's body within its in-memory representation.
    class var bucketBodyOffset: Int {
      let size = MemoryLayout<UInt>.size * bitmapCount
      return size + size % MemoryLayout<T>.alignment
    }

    /// The maximum number of elements that can be stored inside of a bucket.
    class var bucketCapacity: Int { bitmapCount * UInt.bitWidth }

    /// The number of bitmaps in a bucket's header.
    class var bitmapCount: Int { (Int(SlotIndex.max) + 1) / UInt.bitWidth }

  }

  private var storage: _Storage

  public init(minimumCapacity: Int = 0) {
    storage = _Storage(minimumCapacity: minimumCapacity)
  }

  @discardableResult
  public mutating func insert(_ newElement: T) -> Index {
    if !isKnownUniquelyReferenced(&storage) {
      storage = _Storage(copying: storage, minimumCapacity: storage.count + 1)
    }
    return storage.insert(newElement)
  }

  @discardableResult
  public mutating func remove(at position: Index) -> T {
    if !isKnownUniquelyReferenced(&storage) {
      storage = _Storage(copying: storage)
    }
    return storage.remove(at: position)
  }

}

extension Slab.Index: Equatable {}

extension Slab.Index: Hashable {}

extension Slab.Index: Comparable {

  public static func < (lhs: Slab.Index, rhs: Slab.Index) -> Bool {
    return lhs.bucket == rhs.bucket
      ? lhs.slot < rhs.slot
      : lhs.bucket < rhs.bucket
  }

}

extension Slab: MutableCollection {

  public var count: Int {
    return storage.count
  }

  public var isEmpty: Bool {
    return storage.count == 0
  }

  public var startIndex: Index {
    for b in 0 ..< storage.bucketCount {
      if let s = storage.firstSlot(in: b) {
        return Index(bucket: b, slot: SlotIndex(truncatingIfNeeded: s))
      }
    }
    return endIndex
  }

  public var endIndex: Index {
    return Index(bucket: storage.bucketCount, slot: 0)
  }

  public func index(after position: Index) -> Index {
    // Look for the next busy slot in the same bucket.
    if let index = storage.withBucket(position.bucket, { (header, _) -> Index? in
      let s = Int(position.slot)
      let i = s / UInt.bitWidth
      let busymap = header[i] & (~0 << (s % UInt.bitWidth + 1))
      if busymap != 0 {
        let lsb = busymap & ~(busymap - 1)
        return Index(
          bucket: position.bucket,
          slot: SlotIndex(truncatingIfNeeded: Int(log2_64(lsb)) + i * UInt.bitWidth))
      }

      for j in (i + 1) ..< _Storage.bitmapCount where header[i] != 0 {
        let busymap = header[j]
        if busymap != 0 {
          let lsb = busymap & ~(busymap - 1)
          return Index(
            bucket: position.bucket,
            slot: SlotIndex(truncatingIfNeeded: Int(log2_64(lsb)) + j * UInt.bitWidth))
        }
      }

      return nil
    }) { return index }

    // Search in the remaining buckets.
    for b in (position.bucket + 1) ..< storage.bucketCount {
      if let s = storage.firstSlot(in: b) {
        return Index(bucket: b, slot: SlotIndex(truncatingIfNeeded: s))
      }
    }

    return endIndex
  }

  public subscript(position: Index) -> T {
    get {
      return storage.withBucket(position.bucket, { (header, body) in
        let s = Int(position.slot)
        let i = s / UInt.bitWidth
        precondition(header[i] & (1 << (s % UInt.bitWidth)) != 0, "invalid index")
        return body[s]
      })
    }

    set {
      if !isKnownUniquelyReferenced(&storage) {
        storage = _Storage(copying: storage, minimumCapacity: storage.count + 1)
      }

      storage.withBucket(position.bucket, { (header, body) in
        let s = Int(position.slot)
        let i = s / UInt.bitWidth
        precondition(header[i] & (1 << (s % UInt.bitWidth)) != 0, "invalid index")
        body[s] = newValue
      })
    }

    _modify {
      if !isKnownUniquelyReferenced(&storage) {
        storage = _Storage(copying: storage, minimumCapacity: storage.count + 1)
      }

      let s = Int(position.slot)
      let element = storage.withBucket(
        position.bucket, { (header, body) -> UnsafeMutablePointer<T> in
          let i = s / UInt.bitWidth
          precondition(header[i] & (1 << (s % UInt.bitWidth)) != 0, "invalid index")
          return body.advanced(by: s)
        })

      yield &element.pointee
    }
  }

}

fileprivate extension Int {

  /// Returns the closest multiple of `value` whose magnitude is greater than or equal to that of
  /// this integer.
  func roundedAwayFromZero(toNearestMultipleOf value: Int) -> Int {
    return self % value == 0
      ? self
      : self + (value - self % value)
  }

}

// https://stackoverflow.com/a/23000588/6908155
fileprivate func log2_64(_ n: UInt) -> UInt {
#if arch(x86_64) || arch(arm64)
  var value = n
#else
  var value = UInt64(n)
#endif
  value |= value &>> 1
  value |= value &>> 2
  value |= value &>> 4
  value |= value &>> 8
  value |= value &>> 16
  value |= value &>> 32
  let i = (value &* 0x03f6eaf2cd271461) &>> 58
  return UInt(tab64[Int(i)])
}

fileprivate let tab64: [Int8] = [
  0, 58, 1, 59, 47, 53, 2, 60, 39, 48, 27, 54, 33, 42, 3, 61,
  51, 37, 40, 49, 18, 28, 20, 55, 30, 34, 11, 43, 14, 22, 4, 62,
  57, 46, 52, 38, 26, 32, 41, 50, 36, 17, 19, 29, 10, 13, 21, 56,
  45, 25, 31, 35, 16, 9, 12, 44, 24, 15, 8, 23, 7, 6, 5, 63
]
