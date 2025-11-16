import Foundation
import FrontEnd
import Collections
import IR

struct CodePointer {

  var module: Module.ID
  var instructionInModule: InstructionID

}

/// The local variables, parameters, and return address for a function
/// call.
struct StackFrame {

  /// Function parameters
  var parameters: [Address]

  /// The results of instructions.
  var registers: [InstructionID: Any] = [:]

  /// The program counter to which execution should return when
  /// popping this frame.
  var returnAddress: CodePointer

  /// Memory addresses of local variables currently allocated on stack frame
  /// such that last element points to latest allocated local variable.
  var localVariables: [Memory.Address] = []

}

/// Address to object with type layout.
struct Address {

  /// Address of object in memory.
  public let memoryAddress: Memory.Address

  /// Type layout of object.
  public let memoryLayout: TypeLayout

}

extension UnsafeRawPointer {

  /// Returns the number of bytes from `self` to the nearest address
  /// aligned to `a`.
  func offsetToAlignment(_ a: Int) -> Int {
    let b = UInt(bitPattern: self)
    return Int(b.rounded(upToNearestMultipleOf: UInt(a)) - b)
  }

}

extension UnsafeRawBufferPointer {

  /// Returns the number of bytes from the notional base address to
  /// the nearest address aligned to `a`.
  ///
  /// If `self.baseAddress == 0`, returns `0`.
  func firstOffsetAligned(to a: Int) -> Int {
    return baseAddress?.offsetToAlignment(a) ?? 0
  }

}

/// A virtual machine that executes Hylo's in-memory IR representation.
public struct Interpreter {

  /// The program to be executed.
  private let program: IR.Program

  /// The stack- and dynamically-allocated memory in use by the program.
  private var memory = Memory()

  /// Local variables, function parameters and return addresses.
  private var stackFrames: [StackFrame] = []

  /// Identity of the next instruction to be executed.
  private var programCounter: CodePointer

  /// True iff the program is still running.
  public private(set) var isRunning: Bool = true

  public private(set) var standardOutput: String = ""

  public private(set) var standardError: String = ""

  private var typeLayout: TypeLayoutCache

  private var topOfStack: StackFrame {
    get { stackFrames.last! }
    _modify {
      yield &stackFrames[stackFrames.count - 1]
    }
  }

  /// An instance executing `p`.
  ///
  /// - Precondition: `p.entry != nil`
  public init(_ p: IR.Program) {
    program = p
    let entryModuleID = p.entry!
    let entryModule = p.modules[entryModuleID]!
    let entryFunctionID = entryModule.entryFunction!
    let entryFunction = entryModule.functions[entryFunctionID]!
    let entryBlockID = entryFunction.entry!
    let entryInstructionAddress = entryFunction.blocks[entryBlockID].instructions.firstAddress!
    programCounter = .init(
      module: entryModuleID,
      instructionInModule: InstructionID(entryFunctionID, entryBlockID, entryInstructionAddress))

    // The return address of the bottom-most frame will never be used,
    // so we fill it with something arbitrary.
    stackFrames.append(StackFrame(parameters: [], returnAddress: programCounter))
    typeLayout = .init(typesIn: p.base, for: UnrealABI())
  }

  private var currentRegister: Any? {
    get { topOfStack.registers[programCounter.instructionInModule]! }
    set { topOfStack.registers[programCounter.instructionInModule] = newValue }
  }

  /// Executes a single instruction.
  public mutating func step() throws {
    print("\(currentInstruction.site.gnuStandardText): \(currentInstruction)")
    switch currentInstruction {
    case let x as Access:
      currentRegister = toAddress(x.source)!
    case let x as AddressToPointer:
      _ = x
    case let x as AdvancedByBytes:
      _ = x
    case let x as AdvancedByStrides:
      _ = x

    case let x as AllocStack:
      let allocatedAddress = allocate(typeLayout[x.allocatedType])
      topOfStack.localVariables.append(allocatedAddress.memoryAddress)
      currentRegister = allocatedAddress

    case let x as Branch:
      jumpTo(x.target)
      return
    case let x as Call:
      stackFrames.append(
        StackFrame(parameters: blockParams(from: x), returnAddress: try nextCodePointer()))
      jumpTo((x.callee.constant as! FunctionReference).function)
      return
    case let x as CallBuiltinFunction:
      _ = x
    case is CallBundle:
      fatalError("Interpreter: CallBundle instructions have not been removed.")
    case let x as CallFFI:
      _ = x
    case let x as CaptureIn:
      _ = x
    case let x as CloseCapture:
      _ = x
    case let x as CloseUnion:
      _ = x
    case let x as CondBranch:
      let cond = toBuiltinValue(x.condition)!
      if cond.bool! {
        jumpTo(x.targetIfTrue)
      } else {
        jumpTo(x.targetIfFalse)
      }
      return
    case let x as ConstantString:
      _ = x
    case let x as DeallocStack:
      try deallocate(topOfStack.registers[x.location.instruction!]! as! Address)
      topOfStack.localVariables.removeLast()
    case is EndAccess:
      // No effect on program state
      break
    case let x as EndProject:
      _ = x
    case let x as GenericParameter:
      _ = x
    case let x as GlobalAddr:
      _ = x
    case let x as Load:
      let address = toAddress(x.source)!;
      currentRegister = builtIn(at: address)!
    case is MarkState:
      // No effect on program state
      break
    case let x as MemoryCopy:
      let sourceAddress = toAddress(x.source)!
      let destinationAddress = toAddress(x.target)!
      memory.copy(
        byteCount: sourceAddress.memoryLayout.size, from: sourceAddress.memoryAddress,
        to: destinationAddress.memoryAddress)
    case is Move:
      fatalError("Interpreter: Move instructions have not been removed.")
    case let x as OpenCapture:
      _ = x
    case let x as OpenUnion:
      _ = x
    case let x as PointerToAddress:
      _ = x
    case let x as Project:
      _ = x
    case is ProjectBundle:
      fatalError("Interpreter: ProjectBundle instructions have not been removed.")
    case is ReleaseCaptures:
      // No effect on program state
      break
    case is Return:
      popStackFrame()
      return
    case let x as Store:
      store(toBuiltinValue(x.object)!, at: toAddress(x.target)!)
    case let x as SubfieldView:
      let parent = toAddress(x.recordAddress)!;
      currentRegister = address(of: x.subfield, in: parent)
    case let x as Switch:
      _ = x
    case let x as UnionDiscriminator:
      _ = x
    case let x as UnionSwitch:
      _ = x
    case let x as Unreachable:
      _ = x
    case let x as WrapExistentialAddr:
      _ = x
    case let x as Yield:
      _ = x
    default:
      fatalError("Interpreter: unimplemented instruction")
    }
    if stackFrames.isEmpty {
      isRunning = false
    }
    else {
      try advanceProgramCounter()
    }
  }

  /// The instruction at which the program counter points.
  ///
  /// - Precondition: the program is running.
  public var currentInstruction: any Instruction {
    _read {
      yield program.modules[programCounter.module]!
      .functions[programCounter.instructionInModule.function]!
      .blocks[programCounter.instructionInModule.block][programCounter.instructionInModule.address]
    }
  }

  /// Moves the program counter to the next instruction.
  mutating func advanceProgramCounter() throws {
    programCounter.instructionInModule = try nextInstruction()
  }

  /// Returns the next instruction.
  func nextInstruction() throws -> InstructionID {
    let b = program.modules[programCounter.module]!
      .functions[programCounter.instructionInModule.function]!
      .blocks[programCounter.instructionInModule.block].instructions
    guard let a = b.address(after: programCounter.instructionInModule.address) else {
      throw IRError()
    }
    return InstructionID(
      programCounter.instructionInModule.function,
      programCounter.instructionInModule.block,
      a
    )
  }

  /// Returns code pointer pointing to next instruction.
  func nextCodePointer() throws -> CodePointer {
    var codePointer = programCounter;
    codePointer.instructionInModule = try nextInstruction();
    return codePointer;
  }

  /// Removes topmost stack frame and points `programCounter` to next instruction
  /// of any previous stack frame, or stops the program if the stack is now empty.
  ///
  /// - Precondition: the program is running.
  mutating func popStackFrame() {
    programCounter = stackFrames.last!.returnAddress
    stackFrames.removeLast()
    if stackFrames.isEmpty {
      isRunning = false
    }
  }

  /// Allocates object of type layout `t` on `memory` and returns address of start of object.
  mutating func allocate(_ t: TypeLayout) -> Address {
    let addr = memory.allocate(t.size, bytesWithAlignment: t.alignment)
    return .init(memoryAddress: addr, memoryLayout: t)
  }

  /// Deallocates object allocated at `a`.
  mutating func deallocate(_ a: Address) throws {
    try memory.deallocate(a.memoryAddress)
  }

  /// Returns address referenced by `operand`, if any.
  func toAddress(_ operand: Operand) -> Address? {
    switch operand {
    case .register(let instruction):
      return topOfStack.registers[instruction] as? Address
    case .parameter(_, let i):
      return topOfStack.parameters[i]
    case .constant:
      return nil
    }
  }

  /// Returns builtin value referenced by `operand`, if any.
  func toBuiltinValue(_ operand: Operand) -> BuiltinValue? {
    switch operand {
    case .register(let instruction):
      return topOfStack.registers[instruction] as? BuiltinValue
    case .parameter:
      return nil;
    case .constant(let c):
      switch c {
      case let x as IntegerConstant:
        return BuiltinValue(withIntegerConstant: x)
      default:
        fatalError("unimplemented constant parsing!!!")
      }
    }
  }

  /// Returns the address of `subField` in the object  at `origin`.
  mutating func address(of subField: RecordPath, in origin: Address) -> Address
  {
    let memoryAddress = origin.memoryAddress;
    var offset = origin.memoryAddress.offset;
    var layout = origin.memoryLayout;
    for i in subField {
      offset += layout.parts[i].offset
      layout = typeLayout[layout.parts[i].type]
    }
    return .init(
      memoryAddress: .init(allocation: memoryAddress.allocation, offset: offset),
      memoryLayout: layout
    )
  }

  /// Returns builtin object (if any) stored at `a`.
  mutating func builtIn(at a: Address)
    -> BuiltinValue?
  {
    let allocation = a.memoryAddress.allocation
    let offset = a.memoryAddress.offset

    return
      switch a.memoryLayout.type.base
    {
    case BuiltinType.i(1): .i1(memory[allocation][offset, type: UInt8.self] != 0)
    case BuiltinType.i(8): .i8(memory[allocation][offset, type: UInt8.self])
    case BuiltinType.i(16): .i16(memory[allocation][offset, type: UInt16.self])
    case BuiltinType.i(32): .i32(memory[allocation][offset, type: UInt32.self])
    case BuiltinType.i(64): .i64(memory[allocation][offset, type: UInt64.self])
    case BuiltinType.i(128): .i128(memory[allocation][offset, type: UInt128.self])
    default: nil
    }
  }

  /// Returns block parameters from operands in `arg`.
  func blockParams(from arg: Call) -> [Address] {
    arg.arguments.map { toAddress($0)! } + [toAddress(arg.output)!]
  }

  /// Moves the program counter to entry point of `f`.
  mutating func jumpTo(_ f: Function.ID) {
    let module = program.module(defining: f)
    let function = program.modules[module]![f]
    let entryBlock = function.entry!;
    let entryInstruction = function.blocks[entryBlock].instructions.firstAddress!
    programCounter = CodePointer(
      module: module,
      instructionInModule: InstructionID(f, entryBlock, entryInstruction)
    )
  }

  /// Moves the program counter to start of `b`.
  mutating func jumpTo(_ b: Block.ID) {
    let function = b.function
    let module = program.module(defining: function)
    let entryBlock = b.address;
    let entryInstruction = program.modules[module]![b.function]
      .blocks[entryBlock]
      .instructions
      .firstAddress!
    programCounter = CodePointer(
      module: module,
      instructionInModule: InstructionID(function, entryBlock, entryInstruction)
    )
  }

  /// Store builtin value `v` at address `a`.
  mutating func store(_ v: BuiltinValue, at a: Address) {
    let alloc = a.memoryAddress.allocation
    let offset = a.memoryAddress.offset
    switch v {
    case .i1(let n): memory[alloc][offset, type: UInt8.self] = n ? 1 : 0
    case .i8(let n): memory[alloc][offset, type: UInt8.self] = n
    case .i16(let n): memory[alloc][offset, type: UInt16.self] = n
    case .i32(let n): memory[alloc][offset, type: UInt32.self] = n
    case .i64(let n): memory[alloc][offset, type: UInt64.self] = n
    case .i128(let n): memory[alloc][offset, type: UInt128.self] = n
    }
  }

}

struct IRError: Error {}
