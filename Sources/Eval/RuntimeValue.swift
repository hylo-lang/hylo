import AST
import VIL

typealias RuntimeValuePointer = UnsafeMutablePointer<RuntimeValue>

/// A runtime value.
enum RuntimeValue {

  init(ofType type: ValType) {
    switch type.dealiased {
    case let pType as ProductType:
      let capacity = pType.decl.storedVars.count
      self = .record(Record(capacity: capacity))

    case let tType as TupleType:
      self = .record(Record(capacity: tType.elems.count))

    case let bgType as BoundGenericType:
      self = RuntimeValue(ofType: bgType.decl.instanceType)

    case is AsyncType:
      self = .junk

    case _ where type.isExistential:
      self = .container(Container())

    default:
      fatalError("failed to create a runtime representation of type '\(type)'")
    }
  }

  /// A unit value.
  static var unit = RuntimeValue.record(Record(capacity: 0))

  /// A undefined, junk value.
  case junk

  /// An explicit error value.
  case error

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

  /// A native value.
  case native(Any)

  func asNative<T>(ofType: T.Type) -> T {
    guard case .native(let value) = self else { fatalError("unreachable") }
    return value as! T
  }

  var asIntLiteral: Int   { asNative(ofType: Int.self) }
  var asI64       : Int64 { asNative(ofType: Int64.self) }

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
    case .record(let value):
      value.delete()
    case .container(let value):
      value.storage?.deinitialize(count: 1)
      value.storage?.deallocate()
    default:
      break
    }
  }

  func copy() -> RuntimeValue {
    switch self {
    case .record(let val):
      return .record(val.copy())
    case .container(let val):
      return .container(val.copy())
    default:
      return self
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

extension RuntimeValue: CustomStringConvertible {

  var description: String {
    switch self {
    case .record(let value):
      return String(describing: value)
    case .container(let value):
      return String(describing: value)
    case .address(let value):
      return String(describing: value)
    case .function(let value):
      return "@\(value.name)"
    case .builtinFunction(let value):
      return "b\"\(value.name)\""
    case .native(let value):
      return "native(\(value)"
    case .junk:
      return "junk"
    case .error:
      return "error"
    case .returnInfo:
      return "retinfo"
    }
  }

}

/// The payload (i.e. phyiscal storage) of a product or tuple type.
///
/// Since all runtime values must fit the same size, the storage of the record has a buffer of
/// runtime values on the heap, which must be deallocated explicitly.
struct Record {

  /// The storage of the record.
  let storage: RuntimeValuePointer?

  /// The number of values contained in the storage.
  let capacity: Int

  init(capacity: Int) {
    self.capacity = capacity
    if capacity > 0 {
      storage = .allocate(capacity: capacity)
      storage!.initialize(repeating: .junk, count: capacity)
    } else {
      storage = nil
    }
  }

  func delete() {
    guard let buffer = storage else { return }

    for i in 0 ..< capacity {
      buffer.advanced(by: i).pointee.delete()
    }
    buffer.deinitialize(count: capacity)
    buffer.deallocate()
  }

  /// Returns a deep copy of this record.
  func copy() -> Record {
    let newRecord = Record(capacity: capacity)
    for i in 0 ..< capacity {
      newRecord.storage![i] = storage![i].copy()
    }
    return newRecord
  }

}

extension Record: CustomStringConvertible {

  var description: String {
    let members = (0 ..< capacity)
      .map({ i in String(describing: storage![i]) })
      .joined(separator: ", ")
    return "(\(members))"
  }

}

/// The address of a memory block in the interpreter.
///
/// An address is represented as a squence of offsets. The first is the offset of the interpreter's
/// runtime stack, while the following are indices of a record or existential container. In other
/// words, the sequence denotes a path in the interpreter's memory tree.
///
/// Note that this essentially emulates a pointer on top of the interpreter's memory architecture.
struct Address: Equatable {

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

  /// The null location.
  static var null = Address(base: -1)

}

extension Address: CustomStringConvertible {

  var description: String {
    let indices = self.indices.map(String.init(describing:)).joined(separator: ".")
    return "addr(\(indices))"
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

extension Container: CustomStringConvertible {

  var description: String {
    if let value = storage?.pointee {
      return "{\(value), \(witness!)}"
    } else {
      return "{}"
    }
  }

}
