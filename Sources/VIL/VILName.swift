/// The name of a VIL object.
///
/// This struct is essentially a pointer to an immutable string buffer, using the value of that
/// pointer as its identity.
public struct VILName {

  private class Data {

    var cString: UnsafeBufferPointer<CChar>

    init(_ name: String) {
      self.cString = name.utf8CString
        .withContiguousStorageIfAvailable({ (bytes) -> UnsafeBufferPointer<CChar> in
          let buf = UnsafeMutableRawBufferPointer.allocate(
            byteCount: bytes.count, alignment: MemoryLayout<CChar>.alignment)
          buf.copyMemory(from: UnsafeRawBufferPointer(bytes))
          return UnsafeBufferPointer(buf.bindMemory(to: CChar.self))
        })!
    }

    deinit {
      cString.deallocate()
    }

  }

  private struct WeakDataRef {

    weak var data: Data?

  }

  private var data: Data

  public init(_ name: String) {
    let h = name.hashValue
    if var bucket = VILName.buckets[h] {
      let rhs = name.utf8CString
      var i = 0

      // Look for a buffer with the same value in the hash table.
      while i < bucket.count {
        if let data = bucket[i].data {
          if (data.cString.count == rhs.count) && zip(data.cString, rhs).allSatisfy(==) {
            self.data = data
            return
          }
          i += 1
        } else {
          bucket.remove(at: i)
        }
      }
    }

    // No result; insert a new entry in the hash table.
    self.data = Data(name)
    VILName.buckets[h, default: []].append(WeakDataRef(data: self.data))
  }

  private static var buckets: [Int: [WeakDataRef]] = [:]

}

extension VILName: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(data.cString.baseAddress)
  }

  public static func == (lhs: VILName, rhs: VILName) -> Bool {
    return lhs.data === rhs.data
  }

}

extension VILName: Comparable {

  public static func < (lhs: VILName, rhs: VILName) -> Bool {
    return String(describing: lhs).lexicographicallyPrecedes(String(describing: rhs))
  }

}

extension VILName: CustomStringConvertible {

  public var description: String {
    if let pointer = data.cString.baseAddress {
      return String(cString: pointer)
    } else {
      return ""
    }
  }

}

extension VILName: CustomReflectable {

  public var customMirror: Mirror {
    return Mirror(reflecting: String(describing: self))
  }

}
