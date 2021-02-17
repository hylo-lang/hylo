import AST
import VIL

/// A VIL code interpreter.
public struct Interpreter {

  /// The AST context in which the interpreter runs.
  public let context: AST.Context

  /// The runtime stack of the interpreter.
  private var stack: [RuntimeValue] = []

  /// The mappings associating local registers to their offset in the stack.
  private var locals: [[RegisterID: Int]] = []

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
    case let inst as AllocStackInst       : return eval(inst: inst)
    case let inst as ApplyInst            : return eval(inst: inst)
    case let inst as RecordMemberAddrInst : return eval(inst: inst)
    case let inst as RecordMemberInst     : return eval(inst: inst)
    case let inst as StoreInst            : return eval(inst: inst)
    case let inst as LoadInst             : return eval(inst: inst)
    case let inst as RetInst              : return eval(inst: inst)
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
    } else if let offset = locals[locals.count - 1][RegisterID(value)] {
      return stack[offset]
    } else {
      fatalError("bad VIL operand")
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
    // Allocate a new record on the stack.
    let record = Record.new(inst.allocatedType)!
    stack.append(.record(record))

    // Map the instruction onto the record's address.
    let address = Address(base: stack.count - 1)
    stack.append(.address(address))
    locals[locals.count - 1][RegisterID(inst)] = stack.count - 1

    return pc.incremented()
  }

  mutating func eval(inst: ApplyInst) -> ProgramCounter? {
    // Evaluate the call's arguments.
    let argvals = inst.args.map(eval(operand:))

    // Apply the function
    switch eval(operand: inst.fun) {
    case .builtinFunction(let decl):
      // The function is built-in.
      stack.append(evalBuiltinApply(funDecl: decl, args: argvals))
      locals[locals.count - 1][RegisterID(inst)] = stack.count - 1
      return pc.incremented()

    case .function(let function):
      // Allocate space for the return value.
      stack.append(.junk)
      locals[locals.count - 1][RegisterID(inst)] = stack.count - 1

      // Save the current program counter. This will be used as a sentinel to delimit stack frames.
      stack.append(.pc(pc))

      // Setup the callee's environment.
      var frame: [RegisterID: Int] = [:]
      for (argval, argref) in zip(argvals, function.arguments) {
        stack.append(argval.copy())
        frame[RegisterID(argref)] = stack.count - 1
      }
      locals.append(frame)

      // Jump inside the callee.
      return ProgramCounter(atStartOf: function.blocks.first!)

    default:
      fatalError("unreachable")
    }
  }

  func evalBuiltinApply(funDecl: FunDecl, args: [RuntimeValue]) -> RuntimeValue {
    switch funDecl.name {
    case "i64_print":
      guard case .i64(let i) = args[0] else { fatalError("bas VIL code") }
      print(i)
      return .unit

    case "i64_trunc_IntLiteral":
      return .i64(Int64(truncatingIfNeeded: args[0].asIntLiteral()))

    default:
      fatalError("undefined built-in function '\(funDecl.name)'")
    }
  }

  mutating func eval(inst: RecordMemberAddrInst) -> ProgramCounter? {
    // Retrieve the address of the record.
    let recordAddress = eval(operand: inst.record).asAddress()

    // Compute the record member's address.
    let memberOffset: Int = (inst.record.type.unwrap as! NominalType).decl.storedVarDecls
      .firstIndex(where: { decl in decl === inst.memberDecl })!
    let memberAddress = recordAddress.appending(offset: memberOffset)

    // Map the instruction onto the member's address.
    stack.append(.address(memberAddress))
    locals[locals.count - 1][RegisterID(inst)] = stack.count - 1

    return pc.incremented()
  }

  mutating func eval(inst: RecordMemberInst) -> ProgramCounter? {
    // Retrieve the record value.
    let record = eval(operand: inst.record).asRecord()

    // Compute the record member's offset.
    let memberOffset: Int = (inst.record.type.unwrap as! NominalType).decl.storedVarDecls
      .firstIndex(where: { decl in decl === inst.memberDecl })!
    let member = record[memberOffset]

    // Map the instruction onto the member.
    stack.append(member.copy())
    locals[locals.count - 1][RegisterID(inst)] = stack.count - 1

    return pc.incremented()
  }

  mutating func eval(inst: StoreInst) -> ProgramCounter? {
    let lv = eval(operand: inst.lvalue).asAddress()
    let rv = eval(operand: inst.rvalue)
    store(value: rv.copy(), at: lv)

    return pc.incremented()
  }

  mutating func eval(inst: LoadInst) -> ProgramCounter? {
    let lv = eval(operand: inst.lvalue).asAddress()
    stack.append(load(from: lv).copy())
    locals[locals.count - 1][RegisterID(inst)] = stack.count - 1

    return pc.incremented()
  }

  mutating func eval(inst: RetInst) -> ProgramCounter? {
    // Search for start of the current stack frame..
    var returnPCIndex = -1
    for i in (0 ..< stack.count).reversed() {
      if case .pc = stack[i] {
        returnPCIndex = i
        break
      }
    }
    guard returnPCIndex > -1 else { return nil }

    // Store the return value.
    stack[returnPCIndex - 1] = eval(operand: inst.value).copy()
    let returnPC = stack[returnPCIndex].asProgramCounter()

    // Deallocate stack-allocated objects and clear the stack frame.
    for value in stack[(returnPCIndex + 1)...] {
      value.delete()
    }
    stack.removeSubrange(returnPCIndex...)
    locals.removeLast()

    return returnPC.incremented()
  }

  // MARK: Helpers

  /// Stores a value at the given address.
  ///
  /// - Parameters:
  ///   - value: A runtime value.
  ///   - address: A memory denoting a location in the interpreter's memory.
  private mutating func store(value: RuntimeValue, at address: Address) {
    if address.offsets.isEmpty {
      stack[Int(truncatingIfNeeded: address.base)] = value
    } else {
      var base = stack[Int(truncatingIfNeeded: address.base)].asRecord()
      base[address.offsets] = value
    }
  }

  /// Stores a value from the given address.
  ///
  /// - Parameter address: A memory denoting a location in the interpreter's memory.
  private func load(from address: Address) -> RuntimeValue {
    let base = stack[Int(truncatingIfNeeded: address.base)]
    if address.offsets.isEmpty {
      return base
    } else {
      return base.asRecord()[address.offsets]
    }
  }

}

fileprivate struct RegisterID: Hashable {

  unowned let value: AnyObject

  init(_ value: AnyObject) {
    self.value = value
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(value))
  }

  static func == (lhs: RegisterID, rhs: RegisterID) -> Bool {
    return lhs.value === rhs.value
  }

}
