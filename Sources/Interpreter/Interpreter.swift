import Foundation
import FrontEnd
import Collections
import IR

/// The position of an instruction in the program.
struct CodePointer {

  /// The module containing `self`.
  var module: Module.ID

  /// The function in `module` indicated by `self`.
  var functionInModule: Function.ID

  /// The position relative to `functionInModule` indicated by `self`.
  var instructionInFunction: InstructionID

}

/// The local variables, parameters, and return address for a function
/// call.
struct StackFrame {

  /// An identifier by which we can look up a frame in a given
  /// Stack.
  // We could just use the frame's index on the stack but we'd like
  // to be able to rigorously detect invalid addresses.
  typealias ID = UUID

  /// The results of instructions.
  var registers: [InstructionID: Any] = [:]

  /// The program counter to which execution should return when
  /// popping this frame.
  var returnAddress: CodePointer

  /// This frame's unique identifier throughout time.
  let id = ID()

  /// The allocations in this stack frame.
  var allocations: [StackAllocation] = []

  /// The position in `allocations` where each allocation could be found (when live).
  var allocationIDToIndex: [StackAllocation.ID: Int] = [:]

  /// Returns the position of the first byte of a newly-allocated block of memory suitable for
  /// storing a `t`.
  mutating func allocate(_ t: TypeLayout) -> Stack.Address {
    let a = StackAllocation(t)
    allocationIDToIndex[a.id] = allocations.count
    allocations.append(a)
    return Stack.Address(memoryLayout: t, frame: id, allocation: a.id, byteOffset: 0)
  }

  /// Deallocates the memory starting at `a`.
  ///
  /// - Precondition: `a` is the most recent address returned by `self.allocate` that has not been
  ///   `deallocate`d.
  mutating func deallocate(_ a: Stack.Address) {
    precondition(
      a.allocation == allocations.last!.id,
      "The latest allocation that has not been deallocated must be deallocated first.")
    precondition(a.frame == id, "Can't deallocate address from a different frame.")
    precondition(a.byteOffset == 0, "Can't deallocate the memory of a subobject.")
    precondition(
      a.memoryLayout.type == allocations.last!.structure.type,
      "Deallocating using address of the wrong type; perhaps this is a subobject?")
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

/// A region of stack memory.
struct StackAllocation {

  /// The identity of some allocation, unique throughout time.
  typealias ID = UUID

  /// The bytes, preceded by zero or more bytes of initial padding for alignment purposes.
  let storage: [UInt8]

  /// The number of bytes in `storage` before `self` logically begins.
  let baseOffset: Int

  /// The number of `bytes` in `self`.
  let size: Int

  /// The layout for which this allocation was made.
  let structure: TypeLayout

  /// An identity, unique throughout time.
  let id = ID()

  /// An instance suitable for storing a value with the given `structure`.
  init(_ structure: TypeLayout) {
    size = structure.bytes.size
    storage = .init(repeating: 0, count: max(0, size + structure.bytes.alignment - 1))
    baseOffset = size == 0 ? 0 : storage.withUnsafeBytes {
      let b = UInt(bitPattern: $0.baseAddress!)
      return Int(b.rounded(upToNearestMultipleOf: UInt(structure.bytes.alignment)) - b)
    }
    self.structure = structure
  }

  /// Returns the result calling `body` on the bytes.
  func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer)->R) -> R {
    storage.withUnsafeBytes { b in body(.init(rebasing: b[baseOffset..<baseOffset+size])) }
  }

}

/// A thread's call stack.
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
  mutating func push(returnAddress: CodePointer) {
    let f = StackFrame(returnAddress: returnAddress)
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

  /// A typed memory location in the stack.
  struct Address {

    /// The type.
    let memoryLayout: TypeLayout

    /// The frame containing the `allocation`.
    let frame: StackFrame.ID

    /// The allocation containing the memory.
    let allocation: StackAllocation.ID

    /// The offset in the allocation.
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

  /// Text written so far to the process' standard output stream.
  public private(set) var standardOutput: String = ""

  /// Text written so far to the process' standard error stream.
  public private(set) var standardError: String = ""

  /// The type layouts that have been computed so far.
  private var typeLayout: TypeLayoutCache

  /// The top stack frame.
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
    programCounter = .init(
      module: entryModuleID,
      functionInModule: entryFunctionID,
      instructionInFunction: entryFunction.firstInstruction(in: entryBlockID)!)

    // The return address of the bottom-most frame will never be used,
    // so we fill it with something arbitrary.
    stack.push(returnAddress: programCounter)
    typeLayout = .init(typesIn: p.base, for: UnrealABI())
  }

  /// The value of the current instruction's result, if it has been computed.
  private var currentRegister: Any? {
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
    case let x as AddressToPointer:
      _ = x
    case let x as AdvancedByBytes:
      _ = x
    case let x as AdvancedByStrides:
      _ = x

    case let x as AllocStack:
      currentRegister = StackAllocation(typeLayout[x.allocatedType])

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
    programCounter = stack.pop()
    if stack.frames.isEmpty {
      isRunning = false
    }
  }

}

/// An indication of malformed IR.
struct IRError: Error {}
