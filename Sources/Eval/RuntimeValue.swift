import AST
import VIL

typealias RuntimeValuePointer = UnsafeMutablePointer<RuntimeValue>

/// A runtime value.
enum RuntimeValue {

  init(ofType type: ValType) {
    if type == type.context.unitType {
      self = .unit
    } else if type.isExistential {
      self = .container(Container())
    } else if let record = Record.new(type) {
      self = .record(record)
    } else {
      fatalError("failed to create a runtime representation of type '\(type)'")
    }
  }

  /// A unit value.
  case unit

  /// The payload of a record, or a tuple.
  case record(Record)

  var asRecord: Record {
    if case .record(let value) = self {
      return value
    }
    fatalError("unreachable")
  }

  /// An existential container.
  case container(Container)

  var asContainer: Container {
    if case .container(let value) = self {
      return value
    }
    fatalError("unreachable")
  }

  /// The address of a runtime value, relative to the stack of the interpreter.
  case address(Address)

  var asAddress: Address {
    if case .address(let value) = self {
      return value
    }
    fatalError("unreachable")
  }

  /// The value of a program counter.
  case returnInfo(ProgramCounter, RegisterID)

  var asReturnInfo: (pc: ProgramCounter, register: RegisterID) {
    if case .returnInfo(let pc, let register) = self {
      return (pc, register)
    }
    fatalError("unreachable")
  }

  /// An integer literal.
  case intLiteral(Int)

  var asIntLiteral: Int {
    if case .intLiteral(let value) = self {
      return value
    }
    fatalError("unreachable")
  }

  /// A built-in 64-bit signed integer value.
  case i64(Int64)

  var asI64: Int64 {
    if case .i64(let value) = self {
      return value
    }
    fatalError("unreachable")
  }

  /// A VIL function.
  case function(Function)

  var asFunction: Function {
    if case .function(let value) = self {
      return value
    }
    fatalError("unreachable")
  }

  /// A built-in function.
  case builtinFunction(FunDecl)

  var asBuiltinFunction: FunDecl {
    if case .builtinFunction(let value) = self {
      return value
    }
    fatalError("unreachable")
  }

  /// A undefined, junk value.
  case junk

  /// An explicit error value.
  case error

  subscript<C>(offsets: C) -> RuntimeValue where C: BidirectionalCollection, C.Element == Int {
    get {
      precondition(!offsets.isEmpty)
      var result = self
      for offset in offsets {
        result = result.storage!.advanced(by: offset).pointee
      }
      return result
    }

    set {
      precondition(!offsets.isEmpty)
      var target = storage
      for offset in offsets.dropLast() {
        target = storage!.advanced(by: offset)
      }
      target!.advanced(by: offsets.last!).pointee = newValue
    }
  }

  var storage: RuntimeValuePointer? {
    get {
      switch self {
      case .record(let record):
        return record.storage
      case .container(let container):
        return container.storage
      default:
        fatalError("unreachable")
      }
    }

    set {
      switch self {
      case .container(var container):
        container.storage = newValue
        self = .container(container)
      default:
        fatalError("unreachable")
      }
    }
  }

  func delete() {
    switch self {
    case .record(let value): value.delete()
    default: break
    }
  }

  func copy() -> RuntimeValue {
    switch self {
    case .record(let val)   : return .record(val.copy())
    case .container(let val): return .container(val.copy())
    default: return self
    }
  }

  // MARK: Debug

  var isAddress: Bool {
    if case .address = self {
      return true
    } else {
      return false
    }
  }

}

/// The payload (i.e. phyiscal storage) of a product or tuple type.
///
/// Since all runtime values must fit the same size, the storage of the record has a buffer of
/// runtime values on the heap, which must be deallocated explicitly.
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

  /// Returns a deep copy of this record.
  func copy() -> Record {
    let newRecord = Record(capacity: capacity)
    for i in 0 ..< capacity {
      newRecord.storage[i] = storage[i].copy()
    }
    return newRecord
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

/// The address of a memory block in the interpreter.
///
/// An address is represented as a squence of offsets. The first is the offset of the interpreter's
/// runtime stack, while the following are indices of a record or existential container. In other
/// words, the sequence denotes a path in the interpreter's memory tree.
///
/// Note that this essentially emulates a pointer on top of the interpreter's memory architecture.
struct Address {

  private var indices: [Int]

  init(indices: [Int]) {
    self.indices = indices
  }

  init<S>(base: Int, offsets: S) where S: Sequence, S.Element == Int {
    indices = [base] + Array(offsets)
  }

  init(base: Int) {
    indices = [base]
  }

  /// An offset in the interpreter's runtime stack.
  var base: Int { indices[0] }

  /// A sequence of offsets denoting a path in the memory tree rooted by the value located at
  /// `base` in the interpreter's runtime stack.
  var offsets: ArraySlice<Int> { indices.dropFirst() }

  /// Returns a copy of this address, appending the given offset at the end of `offsets`.
  ///
  /// - Parameter offset: An offset.
  func appending(offset newOffset: Int) -> Address {
    return Address(indices: indices + [newOffset])
  }

}

/// An existential container.
struct Container {

  var storage: RuntimeValuePointer?

  var witness: ValType?

  /// Returns a deep copy of this container.
  func copy() -> Container {
    guard let value = storage?.pointee else { return self }

    let newStorage = RuntimeValuePointer.allocate(capacity: 1)
    newStorage.initialize(to: value.copy())
    return Container(storage: newStorage, witness: witness)
  }

}
