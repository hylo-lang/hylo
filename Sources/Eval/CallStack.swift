import VIL

/// A call stack.
///
/// Memory is 8 bytes aligned, assuming the native machine has 64-bit integers.
struct CallStack {

  /// An offset in a call stack, tagged with the ID of the thread to which the stack belongs.
  struct Offset: Equatable {

    private var rawValue: UInt64

    init(threadID: VirtualThread.ID, value: Int) {
      assert(threadID >> 16 == 0)
      assert(value >> 48 == 0)
      rawValue = UInt64(truncatingIfNeeded: value) | UInt64(truncatingIfNeeded: threadID) << 48
    }

    /// A thread ID.
    var threadID: VirtualThread.ID {
      get { Int(truncatingIfNeeded: rawValue >> 48) }
      set {
        assert(newValue >> 16 == 0)
        rawValue = rawValue & (~0 >> 16) | UInt64(truncatingIfNeeded: newValue) << 48
      }
    }

    /// The value of the offset.
    var value: Int {
      get { Int(truncatingIfNeeded: rawValue & (1 << 48 - 1)) }
      set {
        assert(newValue >> 48 == 0)
        rawValue = rawValue & ~(1 << 48 - 1) | UInt64(truncatingIfNeeded: newValue)
      }
    }

  }

  fileprivate typealias RTTI = [(offset: Int, type: VILType)]

  fileprivate typealias FrameHeader = (previousFrameOffset: Int, rtti: RTTI)

  /// The identifier of the thread owning this stack.
  fileprivate var threadID: VirtualThread.ID

  /// The memory of the stack.
  fileprivate var memory: UnsafeMutableRawBufferPointer

  /// A pointer to the top of the stack.
  fileprivate var top: Int

  /// The offset of the last frame from the stack's base address.
  fileprivate var lastFrameOffset: Int

  /// Creates a call stack.
  ///
  /// - Parameter initialCapacity: The initial capacity of the stack, in bytes.
  init(threadID: VirtualThread.ID, initialCapacity: Int) {
    let byteCount = max(initialCapacity, MemoryLayout<FrameHeader>.stride)
    self.threadID = threadID
    self.memory = .allocate(byteCount: byteCount, alignment: Self.defaultAlignment)
    self.top = 0
    self.lastFrameOffset = -1
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

  /// Pushes a frame onto the stack.
  mutating func pushFrame() {
    let base = nextOffset(alignedAt: MemoryLayout<FrameHeader>.alignment, from: top)
    if base + MemoryLayout<FrameHeader>.size > memory.count {
      expand(minimumCapacity: base + MemoryLayout<FrameHeader>.size)
    }

    memory.baseAddress!
      .advanced(by: base)
      .initializeMemory(
        as: FrameHeader.self,
        repeating: (previousFrameOffset: lastFrameOffset, rtti: []),
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

  /// Allocates new space at the top of the stack.
  mutating func allocate(type: VILType, layout: DataLayout) -> ValueAddr {
    precondition(!isEmpty, "the stack is empty")

    let base = nextOffset(alignedAt: layout.alignment(of: type), from: top)
    let byteCount = layout.size(of: type)
    if base + byteCount >= memory.count {
      expand(minimumCapacity: base + byteCount)
    }

    // Store the RTTI.
    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)
    header.pointee.rtti.append((offset: base - lastFrameOffset, type: type))

    // Zero-initialize the allocated memory.
    memory.baseAddress!.advanced(by: base)
      .initializeMemory(as: UInt8.self, repeating: 0, count: byteCount)

    let addr = ValueAddr.stack(Offset(threadID: threadID, value: base))
    top = base + byteCount
    return addr
  }

  /// Deallocates a value from the top of the stack.
  mutating func pop(type: VILType) {
    precondition(!isEmpty, "the stack is empty")

    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)

    guard let last = header.pointee.rtti.last,
          last.type == type
    else { fatalError("shallowest value of the stack does not have the expected type") }

    header.pointee.rtti.removeLast()
    top -= (lastFrameOffset + last.offset)
  }

  /// Copies `count` bytes of from the location pointed to by `src` directly to the memory pointed
  /// to by `dst`.
  mutating func copyMemory(to dst: Offset, from src: Offset, count: Int) {
    precondition(dst.threadID == threadID, "destination address is in a different stack")
    precondition(dst.value + count <= top, "destination address is out of range")
    precondition(src.threadID == threadID, "source address is in a different stack")
    precondition(src.value + count <= top, "source address is out of range")
    memory.baseAddress!
      .advanced(by: dst.value)
      .copyMemory(from: memory.baseAddress!.advanced(by: src.value), byteCount: count)
  }

  /// Returns a buffer with the contents of the stack from the given offset.
  ///
  /// - Warning: The returned buffer is created over the internal memory of the stack and will be
  ///   invalidated by any mutating operation on the stack.
  ///
  /// - Parameters:
  ///   - offset: An offset from the base address of this stack.
  ///   - byteCount: The number of bytes to access.
  func unsafeRawBufferPointer(from offset: Offset, byteCount: Int) -> UnsafeRawBufferPointer {
    precondition(offset.threadID == threadID, "address is in a different stack")
    precondition(offset.value + byteCount <= top, "address is out of range")
    return UnsafeRawBufferPointer(rebasing: memory[offset.value ..< offset.value + byteCount])
  }

  /// Calls the given closure with a mutable pointer referencing the memory at the given offset.
  ///
  /// - Parameters:
  ///   - offset: An offset from the base address of this stack.
  ///   - body: A closure that takes a mutable pointer referencing the memory at the given offset.
  ///     The argument is valid only for the duration of the closure's execution.
  mutating func withUnsafeMutableRawPointer<R>(
    from offset: Offset, _ body: (UnsafeMutableRawPointer) -> R
  ) -> R {
    precondition(offset.threadID == threadID, "address is in a different stack")
    precondition(offset.value <= top, "address is out of range")
    return body(memory.baseAddress!.advanced(by: offset.value))
  }

  /// Expands the size of the stack.
  private mutating func expand(minimumCapacity: Int) {
    let byteCount = max(minimumCapacity, memory.count * 2)
    let newMemory = UnsafeMutableRawBufferPointer.allocate(byteCount: byteCount, alignment: 8)
    newMemory.copyBytes(from: memory)
    memory.deallocate()
    memory = newMemory
  }

  private static let defaultAlignment = MemoryLayout<FrameHeader>.alignment

}

// MARK: CustomReflectable

extension CallStack: CustomReflectable {

  var customMirror: Mirror {
    var children: [Mirror.Child] = []

    var start = lastFrameOffset
    var end = top
    while start != -1 {
      let frame = CallFrameReflection(memory: memory, start: start, byteCount: end - start)
      children.append((label: "+\(start)", value: frame))
      end = start
      start = frame.previousFrameOffset
    }

    return Mirror(self, children: children, displayStyle: .collection)
  }

}

extension CallStack.Offset: CustomReflectable {

  var customMirror: Mirror {
    return Mirror(self, children: ["threadID": threadID, "value": value])
  }

}

fileprivate struct CallFrameReflection: CustomReflectable {

  let previousFrameOffset: Int

  let children: [(label: String, value: CallFrameCellReflection)]

  init(memory: UnsafeMutableRawBufferPointer, start: Int, byteCount: Int) {
    let header = memory.baseAddress!
      .advanced(by: start)
      .load(as: CallStack.FrameHeader.self)

    var children: [(String, CallFrameCellReflection)] = []
    for i in 0 ..< header.rtti.count {
      let (offset, type) = header.rtti[i]
      let upper = i < (header.rtti.count - 1) ? header.rtti[i + 1].offset : byteCount
      let slice = memory[(start + offset) ..< (start + upper)]
      let value = CallFrameCellReflection(
        contents: UnsafeRawBufferPointer(rebasing: slice), type: type)

      children.append((label: "+\(start + offset)", value: value))
    }

    self.previousFrameOffset = header.previousFrameOffset
    self.children = children.reversed()
  }

  var customMirror: Mirror {
    return Mirror(self, children: children, displayStyle: .collection)
  }

}

fileprivate struct CallFrameCellReflection: CustomReflectable {

  let contents: UnsafeRawBufferPointer

  let type: VILType

  var customMirror: Mirror {
    var buf = contents.prefix(8)
      .map({ (byte) -> String in
        let s = String(byte, radix: 16)
        return byte < 16 ? "0\(s)" : s
      })
      .joined(separator: " ")
    if contents.count > 8 { buf += "... and \(contents.count - 8) more bytes" }
    return Mirror(self, children: ["type": type, "contents": buf])
  }

}
