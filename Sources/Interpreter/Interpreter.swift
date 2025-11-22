import Foundation
import FrontEnd
import Collections
import IR

struct CodePointer {

  var module: Module.ID
  var instructionInModule: InstructionID

}

/// The value produced by executing an instruction.
typealias InstructionResult = Any

/// The local variables, parameters, and return address for a function
/// call.
struct StackFrame {
  /// The results of instructions.
  var registers: [InstructionID: InstructionResult] = [:]

  /// The program counter to which execution should return when
  /// popping this frame.
  var returnAddress: CodePointer

  /// The allocations in this stack frame.
  var allocations: [Memory.Address] = []
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

  /// Local variables, parameters, and return addresses.
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
    stackFrames.append(StackFrame(returnAddress: programCounter))
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
    case is Access:
      // No effect on program state
      break
    case let x as AddressToPointer:
      _ = x
    case let x as AdvancedByBytes:
      _ = x
    case let x as AdvancedByStrides:
      _ = x

    case let x as AllocStack:
      _ = x

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
      currentRegister = x.value
    case let x as DeallocStack:
      _ = x
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
      _ = x
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

  /// Removes topmost stack frame and points `programCounter` to next instruction
  /// of any previous stack frame, or stops the program if the stack is now empty.
  ///
  /// - Precondition: the program is running.
  mutating func popStackFrame() {
    programCounter = stackFrames.popLast()!.returnAddress
    if stackFrames.isEmpty {
      isRunning = false
    }
  }

}

struct IRError: Error {}
