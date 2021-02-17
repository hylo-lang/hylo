import AST
import VIL

typealias RuntimeValuePointer = UnsafeMutablePointer<RuntimeValue>

enum RuntimeValue {

  /// A unit value.
  case unit

  /// The payload of the instance of a product type, or the instance of a tuple.
  case record(Record)

  /// The address of a runtime value, relative to the stack of the interpreter.
  case address(Address)

  /// The value of a program counter.
  case pc(ProgramCounter)

  /// An integer literal.
  case intLiteral(Int)

  /// A built-in 64-bit signed integer value.
  case i64(Int64)

  /// A VIL function.
  case function(Function)

  /// A built-in function.
  case builtinFunction(FunDecl)

  /// A undefined, junk value.
  case junk

  /// An explicit error value.
  case error

  func asRecord() -> Record {
    guard case .record(let value) = self else { fatalError("bad VIL code") }
    return value
  }

  func asAddress() -> Address {
    guard case .address(let value) = self else { fatalError("bad VIL code") }
    return value
  }

  func asIntLiteral() -> Int {
    guard case .intLiteral(let value) = self else { fatalError("bad VIL code") }
    return value
  }

  func asProgramCounter() -> ProgramCounter {
    guard case .pc(let value) = self else { fatalError("bad VIL code") }
    return value
  }

  func delete() {
    switch self {
    case .record(let value) : value.delete()
    case .address(let value): value.delete()
    default: break
    }
  }

  func copy() -> RuntimeValue {
    switch self {
    case .record(let value) : return .record(value.copy())
    case .address(let value): return .address(value.copy())
    default: return self
    }
  }

}

struct Address {

  /// The base of the address. This is an index in the interpreter's stack.
  let base: Int32

  /// The offset(s) of the member denoted by the address.
  let offsets: UnsafeMutableBufferPointer<Int32>

  init(base: Int32, offsets: UnsafeMutableBufferPointer<Int32>) {
    self.base = base
    self.offsets = offsets
  }

  init(base: Int) {
    self.base = Int32(truncatingIfNeeded: base)
    self.offsets = UnsafeMutableBufferPointer(start: nil, count: 0)
  }

  init<C>(base: Int, offsets: C) where C: Collection, C.Element == Int {
    self.base = Int32(truncatingIfNeeded: base)
    self.offsets = .allocate(capacity: offsets.count)
    for (i, offset) in offsets.enumerated() {
      self.offsets[i] = Int32(truncatingIfNeeded: offset)
    }
  }

  func delete() {
    self.offsets.deallocate()
  }

  func copy() -> Address {
    let buffer = UnsafeMutableBufferPointer<Int32>.allocate(capacity: offsets.count)
    _ = buffer.initialize(from: offsets)
    return Address(base: base, offsets: buffer)
  }

  func appending(offset newOffset: Int) -> Address {
    let buffer = UnsafeMutableBufferPointer<Int32>.allocate(capacity: offsets.count + 1)
    _ = buffer.initialize(from: offsets)
    buffer[offsets.count] = Int32(truncatingIfNeeded: newOffset)
    return Address(base: base, offsets: buffer)
  }

}

struct Record {

  /// The storage of the record.
  let storage: RuntimeValuePointer

  /// The number of values contained in the storage.
  let capacity: Int

  private init(capacity: Int) {
    self.capacity = capacity
    storage = .allocate(capacity: capacity)
    storage.initialize(repeating: .junk, count: capacity)
  }

  func delete() {
    for i in 0 ..< capacity {
      storage.advanced(by: i).pointee.delete()
    }
    storage.deinitialize(count: capacity)
    storage.deallocate()
  }

  func copy() -> Record {
    let newRecord = Record(capacity: capacity)
    for i in 0 ..< capacity {
      newRecord.storage[i] = storage[i].copy()
    }
    return newRecord
  }

  subscript(offset: Int) -> RuntimeValue {
    get { storage.advanced(by: offset).pointee }
    set { storage.advanced(by: offset).pointee = newValue }
  }

  subscript(offsets: UnsafeMutableBufferPointer<Int32>) -> RuntimeValue {
    get {
      var result = RuntimeValue.record(self)
      for offset in offsets {
        let base = result.asRecord()
        result = base.storage.advanced(by: Int(truncatingIfNeeded: offset)).pointee
      }
      return result
    }

    set {
      precondition(offsets.count > 0)
      var target = self
      for offset in offsets.dropLast() {
        target = target.storage.advanced(by: Int(truncatingIfNeeded: offset)).pointee.asRecord()
      }
      target.storage.advanced(by: Int(truncatingIfNeeded: offsets.last!)).pointee = newValue
    }
  }

  static func new(_ type: ValType) -> Record? {
    if let pType = type as? ProductType {
      return Record(capacity: pType.decl.storedVarDecls.count)
    }

    if let tType = type as? TupleType {
      precondition(!tType.elems.isEmpty, "empty tuple should not be allocated")
      return Record(capacity: tType.elems.count)
    }

    return nil
  }

}
