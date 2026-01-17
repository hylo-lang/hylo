import Foundation
import FrontEnd
import Collections
import IR
import Utils

/// The position of an instruction in the program.
struct CodePointer {

  /// The module containing `self`.
  var module: Module.ID

  /// The function in `module` indicated by `self`.
  var functionInModule: Function.ID

  /// The position relative to `functionInModule` indicated by `self`.
  var instructionInFunction: InstructionID

}

/// The value produced by executing an instruction.
struct InstructionResult {
  /// The instruction's output as an opaque, type-erased value.
  public var payload: Any
}

/// A namespace for notional module-scope declarations that actually
/// can't be accessed without ambiguity in some cases.
///
/// Aliases for these names are normally placed at module scope for ease-of-use.
enum ModuleScope {

  /// A typed location in memory.
  public struct Address: Regular {

    /// The position in memory.
    public let startLocation: Memory.Address

    /// The type to be accessed at `startLocation`.
    public let type: TypeLayout

  }

}

/// A typed location in memory.
typealias Address = ModuleScope.Address

/// The local variables, parameters, and return address for a function
/// call.
struct StackFrame {
  /// The results of instructions.
  public var registers: [InstructionID: InstructionResult] = [:]

  /// The program counter to which execution should return when
  /// popping this frame.
  public var returnAddress: CodePointer

  /// The allocations in this stack frame.
  public var allocations: [Address] = []
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

extension Memory {
  /// Deallocates the allocated memory at `a`, leaving it deallocated and
  /// rendering `a` unusable for any purpose.
  mutating func deallocate(_ a: ModuleScope.Address) throws {
    precondition(a.startLocation.offset == 0, "Can't deallocate the memory of subobject.")
    precondition(
      allocation[a.startLocation.allocation]?.size == a.type.size,
      "Deallocating using address of the wrong type.")
    try deallocate(a.startLocation)
  }
}

/// A thread's call stack.
struct Stack {

  /// Local variables, parameters, and return addresses.
  private var frames: [StackFrame] = []

  /// Adds a new frame on top with the given `returnAddress`.
  public mutating func push(returnAddress: CodePointer) {
    let f = StackFrame(returnAddress: returnAddress)
    frames.append(f)
  }

  /// Removes the top frame and returns its `returnAddress`.
  public mutating func pop() -> CodePointer {
    let f = frames.last!
    defer {
      frames.removeLast()
    }
    return f.returnAddress
  }

  /// The top stack frame.
  public var top: StackFrame {
    _read {
      precondition(!isEmpty)
      yield frames[frames.count - 1]
    }
    _modify {
      precondition(!isEmpty)
      yield &frames[frames.count - 1]
    }
  }

  /// Boolean indicating whether stack contains atleast 1 stack frame.
  public var isEmpty: Bool {
    frames.isEmpty
  }

}

/// A virtual machine that executes Hylo's in-memory IR representation.
public struct Interpreter {

  /// The program to be executed.
  private let program: IR.Program

  /// The stack- and dynamically-allocated memory in use by the program.
  private var memory = Memory()

  /// Local variables, parameters, and return addresses.
  private var callStack = Stack()

  /// Identity of the next instruction to be executed.
  private var programCounter: CodePointer

  /// True iff the program is still running.
  public private(set) var isRunning: Bool = true

  /// Text written so far to the process' standard output stream.
  public private(set) var standardOutput: String = ""

  /// Text written so far to the process' standard error stream.
  public private(set) var standardError: String = ""

  /// The type layouts that have been computed so far.
  private var typeLayout: TypeLayoutCache

  /// The top stack frame.
  private var topOfStack: StackFrame {
    _read {
      yield callStack.top
    }
    _modify {
      yield &callStack.top
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
    programCounter = .init(
      module: entryModuleID,
      functionInModule: entryFunctionID,
      instructionInFunction: entryFunction.firstInstruction(in: entryBlockID)!)

    // The return address of the bottom-most frame will never be used,
    // so we fill it with something arbitrary.
    callStack.push(returnAddress: programCounter)
    typeLayout = .init(typesIn: p.base, for: UnrealABI())
  }

  /// The value of the current instruction's result, if it has been computed.
  private var currentRegister: InstructionResult? {
    get { topOfStack.registers[programCounter.instructionInFunction] }
    set { topOfStack.registers[programCounter.instructionInFunction] = newValue }
  }

  /// Executes a single instruction.
  public mutating func step() throws {
    print("\(currentInstruction.site.gnuStandardText): \(currentInstruction)")
    switch currentInstruction {
    case is Access:
      // No effect on program state
      break
    case let x as AdvancedByBytes:
      _ = x
    case let x as AdvancedByStrides:
      _ = x

    case let x as AllocStack:
      let a = allocate(typeLayout[x.allocatedType])
      topOfStack.allocations.append(a)
      currentRegister = .init(payload: a)

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
      currentRegister = .init(payload: x.value)
    case let x as DeallocStack:
      let a = addressToBeDeallocated(by: x)
      try deallocateStack(a)
    case is EndAccess:
      // No effect on program state
      break
    case let x as EndProject:
      _ = x
    case let x as GenericParameter:
      _ = x
    case let x as GlobalPlace:
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
    case let x as PlaceToPointer:
      _ = x
    case let x as PointerToPlace:
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
      _ = x
    case let x as Switch:
      _ = x
    case let x as UnionDiscriminator:
      _ = x
    case let x as UnionSwitch:
      _ = x
    case let x as Unreachable:
      _ = x
    case let x as WrapExistentialPlace:
      _ = x
    case let x as Yield:
      _ = x
    default:
      fatalError("Interpreter: unimplemented instruction")
    }
    if callStack.isEmpty {
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
      yield program.modules[programCounter.module]![programCounter.instructionInFunction, in: programCounter.functionInModule]
    }
  }

  /// Moves the program counter to the next instruction.
  mutating func advanceProgramCounter() throws {
    let f = program.modules[programCounter.module]![programCounter.functionInModule]
    let b = f.block(of: programCounter.instructionInFunction)
    guard let a = f.instruction(after: programCounter.instructionInFunction, in: b) else {
      throw IRError()
    }
    programCounter.instructionInFunction = a
  }

  /// Removes topmost stack frame and points `programCounter` to next instruction
  /// of any previous stack frame, or stops the program if the stack is now empty.
  ///
  /// - Precondition: the program is running.
  mutating func popStackFrame() {
    precondition(topOfStack.allocations.isEmpty,
        "All local variables allocations for function must be deallocated before returning.")
    programCounter = callStack.pop()
    if callStack.isEmpty {
      isRunning = false
    }
  }

  /// Allocates memory for an object of type `t` and returns the address.
  mutating func allocate(_ t: TypeLayout) -> Address {
    let a = memory.allocate(t.size, bytesWithAlignment: t.alignment)
    return .init(startLocation: a, type: t)
  }

  /// Deallocates `a` allocated on stack.
  mutating func deallocateStack(_ a: Address) throws {
    precondition(
      a == topOfStack.allocations.last!,
      "The latest allocation that has not been deallocated must be deallocated first.")
    try memory.deallocate(a)
    topOfStack.allocations.removeLast()
  }

  /// Returns the address produced by executing instruction identified by `i` in the current frame,
  /// or `nil` if it didn't produce an address.
  func addressProduced(by i: InstructionID) -> Address? {
    topOfStack.registers[i]?.payload as? Address
  }

  /// Returns the address to be deallocated by `i`.
  func addressToBeDeallocated(by i: DeallocStack) -> Address {
    precondition(i.location.instruction != nil, "DeallocStack must reference a valid instruction.")
    let a = addressProduced(by: i.location.instruction!)
    precondition(a != nil, "Referenced instruction must produce an address.")
    return a!
  }

}

/// An indication of malformed IR.
struct IRError: Error {}
