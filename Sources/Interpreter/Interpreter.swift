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
  var registers: [InstructionID: InstructionResult] = [:]

  /// The program counter to which execution should return when
  /// popping this frame.
  var returnAddress: CodePointer

}

/// The value of a `Builtin` type instance, stripped of type information.
struct UntypedBuiltinValue {

  /// The result of reinterpretation as an unsigned integer value.
  ///
  /// Equivalent to `UInt128(unsafeBitCast<T>(x))`, where `x` is the represented value
  /// and `T` is an unsigned integer of the same size as `x`.  Stores the unsigned
  /// representation of integer types.
  public let asUInt128: UInt128

  /// The size of the value in bytes.
  public let size: Int

}

/// Address to object with type layout.
struct Address {

  /// Address of object in memory.
  public let memoryAddress: Memory.Address

  /// Type layout of object.
  public let memoryLayout: TypeLayout

}

/// Result of an instruction.
enum InstructionResult {

  /// Address of object.
  case address(Address)

  /// The builtin object.
  case builtIn(UntypedBuiltinValue)

  /// Address, if present.
  public var address: Address? {
    switch self {
    case .address(let x):
      return x
    default:
      return nil
    }
  }

  /// Object, if present.
  public var builtIn: UntypedBuiltinValue? {
    switch self {
    case .builtIn(let x):
      return x
    default:
      return nil
    }
  }

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

  /// function parameters, and return addresses.
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

  private var currentRegister: InstructionResult? {
    get { topOfStack.registers[programCounter.instructionInModule]! }
    set { topOfStack.registers[programCounter.instructionInModule] = newValue }
  }

  /// Executes a single instruction.
  public mutating func step() throws {
    print("\(currentInstruction.site.gnuStandardText): \(currentInstruction)")
    switch currentInstruction {
    case let x as Access:
      currentRegister = .address(address(denotedBy: x.source)!)
    case let x as AddressToPointer:
      _ = x
    case let x as AdvancedByBytes:
      _ = x
    case let x as AdvancedByStrides:
      _ = x

    case let x as AllocStack:
      currentRegister = .address(allocate(typeLayout[x.allocatedType]))

    case let x as Branch:
      jumpTo(block: x.target)
      return
    case let x as Call:
      stackFrames.append(
        StackFrame(parameters: blockParams(from: x), returnAddress: try nextCodePointer()))
      jumpToEntry(of: (x.callee.constant as! FunctionReference).function)
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
      let cond = builtIn(denotedBy: x.condition)!
      if cond.asUInt128 != 0 {
        jumpTo(block: x.targetIfTrue)
      } else {
        jumpTo(block: x.targetIfFalse)
      }
      return
    case let x as ConstantString:
      _ = x
    case let x as DeallocStack:
      try deallocate(topOfStack.registers[x.location.instruction!]!.address!)
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
      let address = address(denotedBy: x.source)!;
      currentRegister = .builtIn(builtIn(at: address)!)
    case is MarkState:
      // No effect on program state
      break
    case let x as MemoryCopy:
      memcpy(src: address(denotedBy: x.source)!, dest: address(denotedBy: x.target)!)
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
      let obj = builtIn(denotedBy: x.object)!
      let dest = address(denotedBy: x.target)!
      let destAllocIdx = dest.memoryAddress.allocation
      let val = obj.asUInt128
      memory[destAllocIdx].withMutableUnsafeStorage(dest.memoryAddress.offset) {
        switch obj.size {
        case 1: $0.assumingMemoryBound(to: UInt8.self).pointee = UInt8(truncatingIfNeeded: val)
        case 2: $0.assumingMemoryBound(to: UInt16.self).pointee = UInt16(truncatingIfNeeded: val)
        case 4: $0.assumingMemoryBound(to: UInt32.self).pointee = UInt32(truncatingIfNeeded: val)
        case 8: $0.assumingMemoryBound(to: UInt64.self).pointee = UInt64(truncatingIfNeeded: val)
        case 16: $0.assumingMemoryBound(to: UInt128.self).pointee = UInt128(truncatingIfNeeded: val)
        default: fatalError("Unknown builtin size!")
        }
      }
    case let x as SubfieldView:
      let parent = address(denotedBy: x.recordAddress)!;
      currentRegister = .address(subField(denotedBy: x.subfield, of: parent))
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

  /// Returns address of object denoted by operand, if any.
  func address(denotedBy operand: Operand) -> Address? {
    switch operand {
    case .register(let instruction):
      return topOfStack.registers[instruction]?.address
    case .parameter(_, let i):
      return topOfStack.parameters[i]
    case .constant:
      return nil
    }
  }

  /// Returns untyped builtin value denoted by operand, if any.
  func builtIn(denotedBy operand: Operand) -> UntypedBuiltinValue? {
    switch operand {
    case .register(let instruction):
      return topOfStack.registers[instruction]?.builtIn
    case .parameter:
      return nil;
    case .constant(let c):
      switch c {
      case let x as IntegerConstant:
        return UntypedBuiltinValue(
          asUInt128: UInt128(truncatingIfNeeded: x.value),
          size: (x.value.bitWidth + 7) / 8
        )
      default:
        fatalError("unimplemented constant parsing!!!")
      }
    }
  }

  /// Returns address of subfield denoted by `path` stored in field at `a`.
  mutating func subField(denotedBy path: RecordPath, of a: Address)
    -> Address
  {
    let memoryAddress = a.memoryAddress;
    var offset = a.memoryAddress.offset;
    var layout = a.memoryLayout;
    for i in path {
      offset += layout.components[i].offset
      layout = typeLayout[layout.components[i].type]
    }
    return .init(
      memoryAddress: .init(allocation: memoryAddress.allocation, offset: offset),
      memoryLayout: layout
    )
  }

  /// Returns builtin object (if any) stored at `a`.
  mutating func builtIn(at a: Address)
    -> UntypedBuiltinValue?
  {
    if !a.memoryLayout.type.isBuiltin {
      return nil
    }
    let allocIdx = a.memoryAddress.allocation
    let offset = a.memoryAddress.offset
    let size = a.memoryLayout.size;

    let value =
      memory[allocIdx].withUnsafeStorage(offset) {
        switch size {
        case 1: UInt128($0.assumingMemoryBound(to: UInt8.self).pointee)
        case 2: UInt128($0.assumingMemoryBound(to: UInt16.self).pointee)
        case 4: UInt128($0.assumingMemoryBound(to: UInt32.self).pointee)
        case 8: UInt128($0.assumingMemoryBound(to: UInt64.self).pointee)
        case 16: UInt128($0.assumingMemoryBound(to: UInt128.self).pointee)
        default: fatalError("Unknown builtin size!")
        }
    }

    return UntypedBuiltinValue.init(asUInt128: value, size: size);
  }

  /// Copies bytes from object at `src` to bytes of object at `dest`.
  ///
  /// Precondition: `src` and `dest` have same type.
  mutating func memcpy(src: Address, dest: Address) {
    let size = src.memoryLayout.size;

    let srcAllocIdx = src.memoryAddress.allocation
    let destAllocIdx = dest.memoryAddress.allocation

    memory[srcAllocIdx].withUnsafeStorage(src.memoryAddress.offset) { srcBytes in
      memory[destAllocIdx].withMutableUnsafeStorage(dest.memoryAddress.offset) { destBytes in
        destBytes.copyMemory(from: srcBytes, byteCount: size)
      }
    }
  }

  /// Returns block parameters from operands in `arg`.
  func blockParams(from arg: Call) -> [Address] {
    arg.arguments.map { address(denotedBy: $0)! } + [address(denotedBy: arg.output)!]
  }

  /// Moves the program counter to entry point of `f`.
  mutating func jumpToEntry(of f: Function.ID) {
    let moduleId = program.module(defining: f)
    let function = program.modules[moduleId]![f]
    let entryBlock = function.entry!;
    let entryInstruction = function.blocks[entryBlock].instructions.firstAddress!
    programCounter = CodePointer(
      module: moduleId,
      instructionInModule: InstructionID(f, entryBlock, entryInstruction)
    )
  }

  /// Moves the program counter to start of `b`.
  mutating func jumpTo(block b: Block.ID) {
    let functionId = b.function
    let moduleId = program.module(defining: functionId)
    let function = program.modules[moduleId]![functionId]
    let entryBlock = b.address;
    let entryInstruction = function.blocks[entryBlock].instructions.firstAddress!
    programCounter = CodePointer(
      module: moduleId,
      instructionInModule: InstructionID(functionId, entryBlock, entryInstruction)
    )
  }

}

struct IRError: Error {}
