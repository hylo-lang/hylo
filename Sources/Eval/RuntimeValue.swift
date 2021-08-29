/// A runtime value.
struct RuntimeValue {

  /// A small inline buffer, suitable for storing small runtime values on the host's stack.
  ///
  /// The contents of this struct is designed to occupy exacty two words minus 2 bytes. The two
  /// last bytes are used to store the size of the buffer and a tag in an enum.
  private struct InlineBuffer {

#if arch(x86_64) || arch(arm64) || arch(s390x) || arch(powerpc64) || arch(powerpc64le)
    typealias Bytes = (UInt8, UInt8, UInt8, UInt8,
                       UInt8, UInt8, UInt8, UInt8,
                       UInt8, UInt8, UInt8, UInt8,
                       UInt8, UInt8)
#else
    typealias Bytes = (UInt8, UInt8, UInt8, UInt8,
                       UInt8, UInt8)
#endif

    var bytes: Bytes

    var count: UInt8

    init(copying buffer: UnsafeRawBufferPointer) {
      assert(buffer.count <= MemoryLayout<Bytes>.size)
#if arch(x86_64) || arch(arm64) || arch(s390x) || arch(powerpc64) || arch(powerpc64le)
      bytes = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#else
      bytes = (0, 0, 0, 0, 0)
#endif
      count = UInt8(buffer.count)

      if !buffer.isEmpty {
        Swift.withUnsafeMutableBytes(of: &bytes, { storage in
          storage.copyMemory(from: buffer)
        })
      }
    }

    static func holds(byteCount: Int) -> Bool {
      return byteCount <= MemoryLayout<Bytes>.size
    }

  }

#if arch(x86_64) || arch(arm64) || arch(s390x) || arch(powerpc64) || arch(powerpc64le)
  private typealias HalfUInt = UInt32
#else
  private typealias HalfUInt = UInt16
#endif

  /// A buffer representing data borrowed from a different buffer.
  ///
  /// This struct stores a pointer to a buffer and uses half of a word to store the number of bytes
  /// borrowed from that pointer.
  private struct BorrowedBuffer {

    var baseAddress: UnsafeRawPointer?

    var count: HalfUInt

    static func holds(byteCount: Int) -> Bool {
      return byteCount <= HalfUInt.max
    }

  }

  /// A buffer of owned data.
  private final class OwnedBuffer {

    var baseAddress: UnsafeMutableRawPointer?

    var count: Int

    init(copying buffer: UnsafeRawBufferPointer) {
      baseAddress = .allocate(byteCount: buffer.count, alignment: MemoryLayout<Int>.alignment)
      count = buffer.count
      if let address = buffer.baseAddress {
        baseAddress!.copyMemory(from: address, byteCount: buffer.count)
      }
    }

    deinit {
      baseAddress?.deallocate()
    }

  }

  /// The actual storage for the runtime value's representation.
  private enum Representation {

    case inline(InlineBuffer)

    case borrowed(BorrowedBuffer)

    case owned(OwnedBuffer)

  }

  private var representation: Representation

  init<T>(copyingRawBytesOf value: T) {
    assert(isTrivial(T.self), "cannot copy bytes of non-trivial type")
    representation = Swift.withUnsafeBytes(of: value, { (bytes) -> Representation in
      if InlineBuffer.holds(byteCount: MemoryLayout<T>.size) {
        return .inline(InlineBuffer(copying: bytes))
      } else {
        return .owned(OwnedBuffer(copying: bytes))
      }
    })
  }

  init(copying buffer: UnsafeRawBufferPointer) {
    if InlineBuffer.holds(byteCount: buffer.count) {
      representation = .inline(InlineBuffer(copying: buffer))
    } else {
      representation = .owned(OwnedBuffer(copying: buffer))
    }
  }

  init(borrowing buffer: UnsafeRawBufferPointer) {
    if BorrowedBuffer.holds(byteCount: buffer.count) {
      representation = .borrowed(
        BorrowedBuffer(baseAddress: buffer.baseAddress, count: HalfUInt(buffer.count)))
    } else {
      representation = .owned(OwnedBuffer(copying: buffer))
    }
  }

  var byteCount: Int {
    switch representation {
    case .inline(let buffer):
      return Int(buffer.count)
    case .borrowed(let buffer):
      return Int(buffer.count)
    case .owned(let buffer):
      return buffer.count
    }
  }

  func open<T>(as: T.Type) -> T {
    return withUnsafeBytes({ return $0.load(as: T.self) })
  }

  func copyMemory(to pointer: UnsafeMutableRawPointer) {
    withUnsafeBytes({ bytes in
      if bytes.count > 0 {
        pointer.copyMemory(from: bytes.baseAddress!, byteCount: bytes.count)
      }
    })
  }

  func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) -> R) -> R {
    switch representation {
    case .inline(let buffer):
      return Swift.withUnsafeBytes(of: buffer.bytes, { bytes in
        body(UnsafeRawBufferPointer(start: bytes.baseAddress, count: Int(buffer.count)))
      })

    case .borrowed(let buffer):
      return body(UnsafeRawBufferPointer(start: buffer.baseAddress, count: Int(buffer.count)))

    case .owned(let buffer):
      return body(UnsafeRawBufferPointer(start: buffer.baseAddress, count: buffer.count))
    }
  }

  static var unit = RuntimeValue(copying: UnsafeRawBufferPointer(start: nil, count: 0))

}
