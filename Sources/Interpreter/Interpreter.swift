import Foundation
import FrontEnd
import Collections
import IR

struct CodePointer {

  var module: Module.ID
  var instructionInModule: InstructionID

}

/// Result of an instruction.
enum InstructionResult {

  /// Address of object on stack.
  case address(Stack.Address)

  /// Object value.
  case object(StackAllocation)

  /// String literal.
  case string(Data)

  /// The address, if any.
  public var address: Stack.Address? {
    switch self {
    case .address(let x):
      return x
    default:
      return nil;
    }
  }

  /// The object, if any.
  public var object: StackAllocation? {
    switch self {
    case .object(let x):
      return x
    default:
      return nil;
    }
  }

  /// The string-literal, if any.
  public var string: Data? {
    switch self {
    case .string(let x):
      return x
    default:
      return nil;
    }
  }
}

/// The local variables, parameters, and return address for a function
/// call.
struct StackFrame {
  /// An identifier by which we can look up a frame in a given
  /// Stack.
  // We could just use the frame's index on the stack but we'd like
  // to be able to rigorously detect invalid addresses.
  typealias ID = UUID

  /// Function parameters
  var parameters: [Stack.Address]

  /// The results of instructions.
  var registers: [InstructionID: InstructionResult] = [:]

  /// The program counter to which execution should return when
  /// popping this frame.
  var returnAddress: CodePointer

  /// This frame's unique identifier throughout time.
  let id = ID()

  /// The allocations in this stack frame.
  var allocations: [StackAllocation] = []
  var allocationIDToIndex: [UUID: Int] = [:]

  mutating func allocate(_ t: TypeLayout) -> Stack.Address {
    let a = StackAllocation(t)
    allocationIDToIndex[a.id] = allocations.count
    allocations.append(a)
    return Stack.Address(memoryLayout: t, frame: id, allocation: a.id, byteOffset: 0)
  }

  mutating func deallocate(_ a: Stack.Address) {
    precondition(
      a.allocation == allocations.last!.id,
      "The latest allocation that has not been deallocated must be deallocated first.")
    precondition(a.frame == id, "Can't deallocate address from a different frame.")
    precondition(a.byteOffset == 0, "Can't deallocate the memory of a subobject.")
    precondition(
      a.memoryLayout.type == allocations.last!.structure.type,
      "Deallocating using address of the wrong type; perhaps this is a subobject?")
    allocationIDToIndex.removeValue(forKey: a.allocation)
    allocations.removeLast()
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

struct StackAllocation {
  typealias ID = UUID

  let storage: [UInt8]
  let baseOffset: Int
  let size: Int
  let structure: TypeLayout
  let id = ID()

  init(_ structure: TypeLayout) {
    size = structure.bytes.size
    storage = .init(repeating: 0, count: max(0, size + structure.bytes.alignment - 1))
    baseOffset = size == 0 ? 0 : storage.withUnsafeBytes {
      let b = UInt(bitPattern: $0.baseAddress!)
      return Int(b.rounded(upToNearestMultipleOf: UInt(structure.bytes.alignment)) - b)
    }
    self.structure = structure
  }

  func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer)->R) -> R {
    storage.withUnsafeBytes { b in body(.init(rebasing: b[baseOffset..<baseOffset+size])) }
  }
}

struct Stack {
  /// Local variables, parameters, and return addresses.
  public fileprivate(set) var frames: [StackFrame] = []

  /// A mapping from the `id`s of stack frames to their index in the stack.
  private var frameIDToIndex: [StackFrame.ID: Int] = [:]

  /// Accesses the given frame.
  subscript(id: StackFrame.ID) -> StackFrame {
    get { frames[frameIDToIndex[id]!] }
    _modify {
      yield &frames[frameIDToIndex[id]!]
    }
  }

  /// Adds a new frame on top with the given `returnAddress`.
  mutating func push(parameters: [Stack.Address], returnAddress: CodePointer) {
    let f = StackFrame(parameters: parameters, returnAddress: returnAddress)
    frameIDToIndex[f.id] = frames.count
    frames.append(f)
  }

  /// Removes the top frame and returns its `returnAddress`.
  mutating func pop() -> CodePointer {
    let f = frames.last!
    defer {
      frameIDToIndex.removeValue(forKey: f.id)
      frames.removeLast()
    }
    return f.returnAddress
  }

  struct Address {
    let memoryLayout: TypeLayout
    let frame: StackFrame.ID
    let allocation: StackAllocation.ID
    let byteOffset: Int
  }
}

/// A virtual machine that executes Hylo's in-memory IR representation.
public struct Interpreter {

  /// The program to be executed.
  private let program: IR.Program

  /// The stack- and dynamically-allocated memory in use by the program.
  private var memory = Memory()

  /// Local variables, parameters, and return addresses.
  private var stack = Stack()

  /// A mapping from the `id`s of stack frames to their index in the stack.
  private var frameIDToIndex: [StackFrame.ID: Int] = [:]

  /// Identity of the next instruction to be executed.
  private var programCounter: CodePointer

  /// True iff the program is still running.
  public private(set) var isRunning: Bool = true

  public private(set) var standardOutput: String = ""

  public private(set) var standardError: String = ""

  private var typeLayout: TypeLayoutCache

  private var topOfStack: StackFrame {
    get { stack.frames.last! }
    _modify {
      yield &stack.frames[stack.frames.count - 1]
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
    stack.push(parameters: [], returnAddress: programCounter)
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
      currentRegister = .address(topOfStack.allocate(typeLayout[x.allocatedType]))

    case let x as Branch:
      _ = x
    case let x as Call:
      _ = x
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
      _ = x
    case let x as ConstantString:
      currentRegister = .string(x.value)
    case let x as DeallocStack:
      topOfStack.deallocate(topOfStack.registers[x.location.instruction!]!.address!)
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
      _ = x
    case is MarkState:
      // No effect on program state
      break
    case let x as MemoryCopy:
      _ = x
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
      _ = x
    case let x as SubfieldView:
      currentRegister = .address(
        address(ofField: x.subfield, at: address(denotedBy: x.recordAddress)!))
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
    if stack.frames.isEmpty {
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
    let b = program.modules[programCounter.module]!
      .functions[programCounter.instructionInModule.function]!
      .blocks[programCounter.instructionInModule.block].instructions
    guard let a = b.address(after: programCounter.instructionInModule.address) else {
      throw IRError()
    }
    programCounter.instructionInModule = InstructionID(
      programCounter.instructionInModule.function,
      programCounter.instructionInModule.block,
      a)
  }

  /// Removes topmost stackframe and points `programCounter` to next instruction
  /// of any previous stackframe, or stops the program if the stack is now empty.
  ///
  /// - Precondition: the program is running.
  mutating func popStackFrame() {
    programCounter = stack.pop()
    if stack.frames.isEmpty {
      isRunning = false
    }
  }

  /// Returns address of object denoted by operand, if any.
  func address(denotedBy operand: Operand) -> Stack.Address? {
    switch operand {
    case .register(let instruction):
      return topOfStack.registers[instruction]?.address
    case .parameter(_, let i):
      return topOfStack.parameters[i]
    case .constant:
      return nil
    }
  }

  /// Returns address of `field` stored at `address` in stack.
  mutating func address(ofField field: RecordPath, at address: Stack.Address) -> Stack.Address {
    let stackframe = stack[address.frame]
    let allocationIndex = stackframe.allocationIDToIndex[address.allocation]!
    let allocation = stackframe.allocations[allocationIndex]
    var offset = 0;
    var layout = allocation.structure;
    for i in field {
      offset += layout.components[i].offset
      layout = typeLayout[layout.components[i].type]
    }
    return Stack.Address.init(
      memoryLayout: layout,
      frame: address.frame,
      allocation: address.allocation,
      byteOffset: offset
    )
  }

}

struct IRError: Error {}
