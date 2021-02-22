import AST
import VIL

/// A VIL code interpreter.
///
/// Memory Management
/// =================
///
/// The interpreter maintains a runtime stack that serves to store stack-allocated memory, function
/// arguments and return addresses, as well as a stack of local frames, which maps each virtual
/// register in scope onto its runtime value.
///
/// All runtime values are implemented with the same size, so that they can fit in a simple Swift
/// array. Unfortunately, an significant drawback of this approach is that we cannot emulate stack
/// allocation accurately. Specifically, the storage of a record has to be allocated in the heap.
public struct Interpreter {

  /// The AST context in which the interpreter runs.
  public let context: AST.Context

  /// The runtime stack of the interpreter.
  private var stack: [RuntimeValue] = []

  /// The values of the local registers.
  private var locals: [[RegisterID: RuntimeValue]] = []

  /// The program counter of the interpreter.
  private var pc = ProgramCounter()

  public init(context: AST.Context) {
    self.context = context
  }

  /// Evaluates the given module.
  public mutating func eval(module: Module) throws {
    // Set the interpreter's program counter at the start of the main function.
    guard let entry = module.functions["main"]?.blocks.first else {
      throw RuntimeError(message: "no entry point")
    }
    pc = ProgramCounter(atStartOf: entry)

    // (Re)initialize the interpreter's state.
    stack  = []
    locals = [[:]]

    // Execute the function.
    while let nextPC = step() {
      pc = nextPC
    }
  }

  /// Executes the instruction pointed by the program counter.
  mutating func step() -> ProgramCounter? {
    // Dispatch the next instrunction to its handler.
    switch pc.inst {
    case let inst as AllocStackInst         : return eval(inst: inst)
    case let inst as AllocExistentialInst   : return eval(inst: inst)
    case let inst as OpenExistentialAddrInst: return eval(inst: inst)
    case let inst as CopyAddrInst           : return eval(inst: inst)
    case let inst as UnsafeCastAddrInst     : return eval(inst: inst)
    case let inst as ApplyInst              : return eval(inst: inst)
    case let inst as RecordMemberAddrInst   : return eval(inst: inst)
    case let inst as RecordMemberInst       : return eval(inst: inst)
    case let inst as TupleInst              : return eval(inst: inst)
    case let inst as StoreInst              : return eval(inst: inst)
    case let inst as LoadInst               : return eval(inst: inst)
    case let inst as RetInst                : return eval(inst: inst)
    case nil:
      fatalError("invalid program counter")
    case .some:
      fatalError("unreachable")
    }
  }

  /// Evaluates the given operand.
  func eval(operand value: Value) -> RuntimeValue {
    if let literal = value as? LiteralValue {
      return eval(literal: literal)
    } else {
      return locals[locals.count - 1][RegisterID(value)]!
    }
  }

  /// Evaluates the given literal.
  func eval(literal value: LiteralValue) -> RuntimeValue {
    switch value {
    case is UnitValue:
      return .unit
    case let literal as IntLiteralValue:
      return .intLiteral(literal.value)
    case let literal as FunRef:
      return .function(literal.function)
    case let literal as BuiltinFunRef:
      return .builtinFunction(literal.decl)
    case is Error:
      return .error
    default:
      fatalError("unreachable")
    }
  }

  mutating func eval(inst: AllocStackInst) -> ProgramCounter? {
    // Allocate a new block of memory on the stack.
    stack.append(RuntimeValue(ofType: inst.allocatedType))

    // Map the instruction onto the record's address.
    let address = Address(base: stack.count - 1)
    locals[locals.count - 1][RegisterID(inst)] = .address(address)

    return pc.incremented()
  }

  mutating func eval(inst: AllocExistentialInst) -> ProgramCounter? {
    // Allocate memory for an existential package.
    let containerAddr = eval(operand: inst.container).asAddress
    var container = load(from: containerAddr).asContainer
    container.storage = .allocate(capacity: 1)
    container.storage!.initialize(to: RuntimeValue(ofType: inst.witness))
    container.witness = inst.witness
    store(.container(container), at: containerAddr)

    // Map the instruction onto the allocated address.
    let address = containerAddr.appending(offset: 0)
    locals[locals.count - 1][RegisterID(inst)] = .address(address)

    return pc.incremented()
  }

  mutating func eval(inst: OpenExistentialAddrInst) -> ProgramCounter? {
    let containerAddr = eval(operand: inst.container).asAddress

    // Map the instruction onto the address of the existential package.
    let address = containerAddr.appending(offset: 0)
    locals[locals.count - 1][RegisterID(inst)] = .address(address)

    return pc.incremented()
  }

  mutating func eval(inst: CopyAddrInst) -> ProgramCounter? {
    let source = eval(operand: inst.source)
    let dest = eval(operand: inst.dest)
    assert(source.isAddress && dest.isAddress)

    store(load(from: source.asAddress), at: dest.asAddress)

    return pc.incremented()
  }

  mutating func eval(inst: UnsafeCastAddrInst) -> ProgramCounter? {
    let addr = eval(operand: inst.source)
    assert(addr.isAddress)

    // FIXME: Check that the layout of the source object is compatible with the target type.
    locals[locals.count - 1][RegisterID(inst)] = addr
    return pc.incremented()
  }

  mutating func eval(inst: ApplyInst) -> ProgramCounter? {
    // Evaluate the call's arguments.
    let argvals = inst.args.map(eval(operand:))

    // Apply the function
    switch eval(operand: inst.fun) {
    case .builtinFunction(let decl):
      // The function is built-in.
      let value = evalBuiltinApply(funDecl: decl, args: argvals)
      locals[locals.count - 1][RegisterID(inst)] = value
      return pc.incremented()

    case .function(let function):
      // Save the return info. This will serve as a sentinel to delimit the start of the call
      // frame, and indicate to the callee where the return value should be stored.
      stack.append(.returnInfo(pc, RegisterID(inst)))

      // Setup the callee's environment.
      var frame: [RegisterID: RuntimeValue] = [:]
      for (argval, argref) in zip(argvals, function.arguments) {
        frame[RegisterID(argref)] = argval
      }
      locals.append(frame)

      // Jump inside the callee.
      return ProgramCounter(atStartOf: function.blocks.first!)

    default:
      fatalError("unreachable")
    }
  }

  func evalBuiltinApply(funDecl: FunDecl, args: [RuntimeValue]) -> RuntimeValue {
    if funDecl.name.starts(with: "i64_") {
      switch funDecl.name.dropFirst(4) {
      case "print":
        guard case .i64(let i) = args[0] else { fatalError("bas VIL code") }
        print(i)
        return .unit

      case "trunc_IntLiteral":
        return .i64(Int64(truncatingIfNeeded: args[0].asIntLiteral))

      case let suffix:
        // The function must take 2 operands.
        let lhs = args[0].asI64
        let rhs = args[1].asI64

        switch suffix {
        case "add": return .i64(lhs + rhs)
        case "sub": return .i64(lhs - rhs)
        case "mul": return .i64(lhs * rhs)
        case "div": return .i64(lhs / rhs)
        case "mod": return .i64(lhs % rhs)
        default: break
        }
      }
    }

    fatalError("undefined built-in function '\(funDecl.name)'")
  }

  mutating func eval(inst: RecordMemberAddrInst) -> ProgramCounter? {
    // Retrieve the address of the record.
    let recordAddress = eval(operand: inst.record).asAddress

    // Compute the record member's address.
    let memberOffset: Int = (inst.record.type.unwrap as! NominalType).decl.storedVarDecls
      .firstIndex(where: { decl in decl === inst.memberDecl })!
    let memberAddress = recordAddress.appending(offset: memberOffset)

    // Map the instruction onto the member's address.
    locals[locals.count - 1][RegisterID(inst)] = .address(memberAddress)

    return pc.incremented()
  }

  mutating func eval(inst: RecordMemberInst) -> ProgramCounter? {
    // Retrieve the record value.
    let record = eval(operand: inst.record)

    // Compute the record member's offset.
    let memberOffset: Int = (inst.record.type.unwrap as! NominalType).decl.storedVarDecls
      .firstIndex(where: { decl in decl === inst.memberDecl })!
    let member = record[[memberOffset]]

    // Map the instruction onto the member.
    locals[locals.count - 1][RegisterID(inst)] = member

    return pc.incremented()
  }

  mutating func eval(inst: TupleInst) -> ProgramCounter? {
    if inst.tupleType == context.unitType {
      locals[locals.count - 1][RegisterID(inst)] = .unit
    } else {
      let record = Record.new(inst.tupleType)!
      locals[locals.count - 1][RegisterID(inst)] = .record(record)
    }

    return pc.incremented()
  }

  mutating func eval(inst: StoreInst) -> ProgramCounter? {
    let lv = eval(operand: inst.lvalue).asAddress
    let rv = eval(operand: inst.rvalue)
    store(rv, at: lv)

    return pc.incremented()
  }

  mutating func eval(inst: LoadInst) -> ProgramCounter? {
    let lv = eval(operand: inst.lvalue).asAddress
    locals[locals.count - 1][RegisterID(inst)] = load(from: lv)

    return pc.incremented()
  }

  mutating func eval(inst: RetInst) -> ProgramCounter? {
    // Search for start of the current stack frame..
    var returnInfoIndex = -1
    for i in (0 ..< stack.count).reversed() {
      if case .returnInfo = stack[i] {
        returnInfoIndex = i
        break
      }
    }
    guard returnInfoIndex > -1 else { return nil }
    let returnInfo = stack[returnInfoIndex].asReturnInfo

    // Store the return value.
    locals[locals.count - 2][returnInfo.register] = eval(operand: inst.value).copy()

    // Deallocate stack-allocated objects and values assigned to return instructions.
    for (registerID, value) in locals.last! {
      switch registerID.value {
      case is RetInst:
        value.delete()
      case is AllocStackInst:
        stack[value.asAddress.base].delete()
      default:
        continue
      }
    }

    // Clear the local frames.
    stack.removeSubrange(returnInfoIndex...)
    locals.removeLast()

    return returnInfo.pc.incremented()
  }

  // MARK: Helpers

  /// Stores a value at the given address.
  ///
  /// - Parameters:
  ///   - value: A runtime value.
  ///   - address: A memory denoting a location in the interpreter's memory.
  private mutating func store(_ value: RuntimeValue, at address: Address) {
    if address.offsets.isEmpty {
      stack[address.base] = value.copy()
    } else {
      // FIXME: There will be a leak here if no other stack cell contains this object.
      var object = stack[address.base].copy()
      object[address.offsets] = value.copy()
      stack[address.base] = object
    }
  }

  /// Loads a value from the given address.
  ///
  /// - Parameter address: An address denoting a location in the interpreter's memory.
  private func load(from address: Address) -> RuntimeValue {
    if address.offsets.isEmpty {
      return stack[address.base]
    } else {
      return stack[address.base][address.offsets]
    }
  }

}
