import Basic
import VIL

/// A table mapping register keys to a value.
typealias RegisterTable = [RegisterTableKey: RegisterEntryInfo]

/// A key in a register table.
enum RegisterTableKey: Hashable {

  /// A key identifying the caller address register.
  case callerAddr

  /// A key identifying a value register.
  case value(Value)

  /// Encodes the key into a raw value, using pointer tagging to represent the `callerAddr` case.
  var rawValue: Int {
    switch self {
    case .callerAddr:
      return 1
    case .value(let inst):
      return Int(bitPattern: ObjectIdentifier(inst))
    }
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(rawValue)
  }

  static func == (lhs: RegisterTableKey, rhs: RegisterTableKey) -> Bool {
    return lhs.rawValue == rhs.rawValue
  }

}

/// A data structure providing information about a specific entry in a register table.
struct RegisterEntryInfo {

  private var rawValue: UInt64

  init(offset: Int, count: Int, busy: Bool) {
    assert(offset >> 48 == 0)
    assert(count >> 15 == 0)
    rawValue = UInt64(truncatingIfNeeded: offset) | UInt64(truncatingIfNeeded: count) << 48
    if busy { rawValue |= (1 << 63) }
  }

  /// The offset of the register from the stack's base address.
  var offset: Int {
    get { Int(truncatingIfNeeded: rawValue & (1 << 48 - 1)) }
    set {
      assert(offset >> 48 == 0)
      rawValue = rawValue & ~(1 << 48 - 1) | UInt64(truncatingIfNeeded: newValue)
    }
  }

  /// The number of bytes stored in the register.
  var count: Int {
    get { Int(truncatingIfNeeded: (rawValue >> 48) & ~(1 << 15)) }
    set {
      assert(count >> 15 == 0)
      rawValue = rawValue & (~0 >> 16 | 1 << 63) | UInt64(truncatingIfNeeded: newValue) << 48
    }
  }

  /// A flag indicating whether the register has been assigned.
  var busy: Bool {
    get { Int(truncatingIfNeeded: rawValue >> 63) != 0 }
    set { rawValue = rawValue & (~0 >> 1) | (newValue ? (1 << 63) : 0) }
  }

}

/// A register stack.
///
/// Each frame is a table of "virtual registers", mapping register keys to their values.
struct RegisterStack {

  fileprivate typealias FrameHeader = (previousFrameOffset: Int, table: RegisterTable)

  /// The memory of the stack.
  fileprivate var memory: UnsafeMutableRawBufferPointer

  /// A pointer to the top of the stack.
  fileprivate var top: Int

  /// The offset of the last frame from the stack's base address.
  fileprivate var lastFrameOffset: Int

  /// Creates a call stack.
  ///
  /// - Parameter initialCapacity: The initial capacity of the stack, in bytes.
  init(initialCapacity: Int) {
    let byteCount = max(initialCapacity, MemoryLayout<FrameHeader>.stride)
    memory = .allocate(byteCount: byteCount, alignment: Self.defaultAlignment)
    top = 0
    lastFrameOffset = -1
  }

  mutating func deinitialize() {
    if memory.isEmpty { return }

    while lastFrameOffset != -1 { removeFrame() }
    memory.deallocate()
    memory = UnsafeMutableRawBufferPointer(start: nil, count: 0)
  }

  /// A Boolean value indicating whether the stack is empty.
  var isEmpty: Bool {
    return lastFrameOffset == -1
  }

  /// Pushes a new frame onto the stack.
  mutating func pushFrame() {
    let base = nextOffset(alignedAt: MemoryLayout<FrameHeader>.alignment, from: top)
    precondition(base + MemoryLayout<FrameHeader>.size < memory.count, "stack overflow")

    memory.baseAddress!
      .advanced(by: base)
      .initializeMemory(
        as: FrameHeader.self,
        repeating: (previousFrameOffset: lastFrameOffset, table: [:]),
        count: 1)
    lastFrameOffset = base
    top = base + MemoryLayout<FrameHeader>.size
  }

  /// Removes the latest frame from the stack, deinitializing its header.
  mutating func removeFrame() {
    precondition(!isEmpty, "the stack is empty")

    // Restore the offset of the previous frame and deinitializes the current one.
    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)
    top = lastFrameOffset
    lastFrameOffset = header.pointee.previousFrameOffset
    header.deinitialize(count: 1)
  }

  mutating func assign(native value: Bool, to key: RegisterTableKey) {
    assign(native: value, ofType: Bool.self, to: key)
  }

  mutating func assign(native value: Int, to key: RegisterTableKey) {
    assign(native: value, ofType: Int.self, to: key)
  }

  mutating func assign(native value: InstAddr, to key: RegisterTableKey) {
    assign(native: value, ofType: InstAddr.self, to: key)
  }

  mutating func assign(native value: ValueAddr, to key: RegisterTableKey) {
    assign(native: value, ofType: ValueAddr.self, to: key)
  }

  mutating func assign(native value: ThickFunction, to key: RegisterTableKey) {
    assign(native: value, ofType: ThickFunction.self, to: key)
  }

  private mutating func assign<T>(native value: T, ofType: T.Type, to key: RegisterTableKey) {
    let ptr = allocate(
      byteCount: MemoryLayout<T>.size,
      alignedAt: MemoryLayout<T>.alignment,
      busy: true,
      forKey: key)
    ptr.initializeMemory(as: T.self, repeating: value, count: 1)
  }

  mutating func assign(value: RuntimeValue, to key: RegisterTableKey) {
    value.withUnsafeBytes({ assign(contentsOf: $0, to: key)})
  }

  mutating func assign(
    contentsOf buffer: UnsafeRawBufferPointer,
    alignedAt alignment: Int = defaultAlignment,
    to key: RegisterTableKey
  ) {
    let ptr = allocate(
      byteCount: buffer.count,
      alignedAt: alignment,
      busy: true,
      forKey: key)

    if buffer.count > 0 {
      ptr.copyMemory(from: buffer.baseAddress!, byteCount: buffer.count)
    }
  }

  mutating func assignNoAlloc(
    contentsOf buffer: UnsafeRawBufferPointer, to key: RegisterTableKey
  ) {
    precondition(!isEmpty, "the stack is empty")

    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)
    let info = header.pointee.table[key] ?< fatalError("register is not reserved")
    precondition(!info.busy, "register is assigned")
    precondition(info.count >= buffer.count, "buffer is too large")

    if buffer.count > 0 {
      let ptr = memory.baseAddress!.advanced(by: info.offset)
      ptr.copyMemory(from: buffer.baseAddress!, byteCount: buffer.count)
    }
    header.pointee.table[key]!.busy = true
  }

  mutating func reserve(byteCount: Int, alignedAt alignment: Int, forKey key: RegisterTableKey) {
    _ = allocate(byteCount: byteCount, alignedAt: alignment, busy: false, forKey: key)
  }

  private mutating func allocate(
    byteCount: Int,
    alignedAt alignment: Int,
    busy: Bool,
    forKey key: RegisterTableKey
  ) -> UnsafeMutableRawPointer {
    precondition(!isEmpty, "the stack is empty")

    // Align the base address of the new allocation.
    let base = nextOffset(alignedAt: alignment, from: top)
    precondition(base + byteCount < memory.count, "stack overflow")

    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)
    precondition(header.pointee.table[key] == nil, "register is reserved")

    // Zero-initialize the allocated memory.
    let ptr = memory.baseAddress!.advanced(by: base)
    ptr.initializeMemory(as: UInt8.self, repeating: 0, count: byteCount)

    header.pointee.table[key] = RegisterEntryInfo(offset: base, count: byteCount, busy: busy)
    top = base + byteCount
    return ptr
  }

  func load<T>(_: T.Type, forKey key: RegisterTableKey) -> T {
    return unsafeRawBufferPointer(forKey: key).load(as: T.self)
  }

  /// Returns a buffer with the contents of the register for the given key.
  ///
  /// - Warning: The returned buffer is created over the internal memory of the stack and will be
  ///   invalidated by any mutating operation on the stack.
  ///
  /// - Parameter key: A register key.
  func unsafeRawBufferPointer(forKey key: RegisterTableKey) -> UnsafeRawBufferPointer {
    precondition(!isEmpty, "the stack is empty")

    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)
    guard let i = header.pointee.table[key] else { fatalError("no value for the given key") }

    return UnsafeRawBufferPointer(rebasing: memory[i.offset ..< i.offset + i.count])
  }

  private func frameOffset(of frameIndex: Int) -> Int {
    var offset = lastFrameOffset
    var index = frameIndex

    while index > 0 {
      offset = memory.baseAddress!
        .advanced(by: offset)
        .assumingMemoryBound(to: FrameHeader.self)
        .pointee.previousFrameOffset
      precondition(offset != -1, "index is out of bound")
      index -= 1
    }
    return offset
  }

  private static let defaultAlignment = MemoryLayout<FrameHeader>.alignment

}

// MARK: CustomReflectable

extension RegisterEntryInfo: CustomReflectable {

  var customMirror: Mirror {
    return Mirror(self, children: ["offset": offset, "count": count, "busy": busy])
  }

}
