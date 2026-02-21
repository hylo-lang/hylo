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

/// A value manipulated by the IR.
struct Value {
  /// The underlying type-erased representation of value.
  public var payload: Any
}

/// The value produced by executing an instruction.
enum InstructionResult {

  /// Produces a value.
  ///
  /// Execution continues at the next instruction in sequence.
  case value(Value)

  /// Transfer control to specific instruction.
  case jump(CodePointer)

}

/// The local variables, parameters, and return address for a function
/// call.
struct StackFrame {
  /// The results of instructions.
  public var registers: [InstructionID: Value] = [:]

  /// The program counter to which execution should return when
  /// popping this frame.
  public var returnAddress: CodePointer

  /// The allocations in this stack frame.
  var allocations: [Memory.Place] = []

  /// Location of values passed to the function.
  var parameters: [Memory.Place]
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

/// A thread's call stack.
struct Stack {

  /// Local variables, parameters, and return addresses.
  private var frames: [StackFrame] = []

  /// Adds a new frame on top with the given `returnAddress` and `parameters`.
  public mutating func push(returnAddress: CodePointer, parameters: [Memory.Place]) {
    let f = StackFrame(returnAddress: returnAddress, parameters: parameters)
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
  private var memory: Memory

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
    callStack.push(returnAddress: programCounter, parameters: [])
    memory = Memory(typesIn: p.base, for: UnrealABI())
  }

  /// Executes a single instruction.
  public mutating func step() throws {
    let r = try stepResult()

    if case .value(let v) = r {
      topOfStack.registers[programCounter.instructionInFunction] = v
    }

    if case .jump(let pc) = r {
      programCounter = pc
      if callStack.isEmpty {
        isRunning = false
      }
      return
    }

    try advanceProgramCounter()
  }

  /// Executes a single instruction without recording its result.
  private mutating func stepResult() throws -> InstructionResult? {
    print("\(currentInstruction.site): \(currentInstruction)")
    switch currentInstruction {
    case let x as Access:
      return .value(.init(payload: asPlace(x.source)))
    case let x as AdvancedByBytes:
      _ = x
    case let x as AdvancedByStrides:
      _ = x

    case let x as AllocStack:
      let a = memory.allocate(x.allocatedType)
      topOfStack.allocations.append(a)
      return .value(.init(payload: a))

    case let x as Branch:
      return .jump(entryPoint(of: x.target))
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
      return .value(.init(payload: x.value))
    case let x as DeallocStack:
      let p = asPlace(x.location)
      try deallocateStack(p)
      return nil
    case is EndAccess:
      // No effect on program state
      return nil
    case let x as EndProject:
      _ = x
    case let x as GenericParameter:
      _ = x
    case let x as GlobalPlace:
      _ = x
    case let x as Load:
      let v = try memory.builtinValue(in: asPlace(x.source));
      return .value(.init(payload: v))
    case is MarkState:
      // No effect on program state
      return nil
    case let x as MemoryCopy:
      try memory.copy(asPlace(x.source), to: asPlace(x.target))
      return nil
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
      return nil
    case is Return:
      return .jump(popStackFrame())
    case let x as Store:
      try memory.store(asBuiltinValue(x.object), in: asPlace(x.target))
      return .none
    case let x as SubfieldView:
      let p = asPlace(x.recordPlace)
      return .value(.init(payload: memory.location(of: x.subfield, in: p)))
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

    unreachable("Unimplemented processing of instruction")
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

  /// Removes topmost stack frame and return code pointer to next instruction of any
  /// previous stack frame.
  ///
  /// - Precondition: the program is running.
  mutating func popStackFrame() -> CodePointer {
    precondition(topOfStack.allocations.isEmpty,
        "All local variables allocations for function must be deallocated before returning.")
    return callStack.pop()
  }

  /// Deallocates `a` allocated on stack.
  mutating func deallocateStack(_ a: Memory.Place) throws {
    precondition(
      a == topOfStack.allocations.last!,
      "The latest allocation that has not been deallocated must be deallocated first.")
    try memory.deallocate(a)
    topOfStack.allocations.removeLast()
  }

  /// Interpret `x` as a Place.
  ///
  /// - Precondition: `x` is a Place.
  func asPlace(_ x: Operand) -> Memory.Place {
    switch x {
    case .register(let instruction):
      topOfStack.registers[instruction]!.payload as! Memory.Place
    case .parameter(_, let i):
      topOfStack.parameters[i]
    case .constant:
      preconditionFailure("Constant operand is not a Place.")
    }
  }

  /// Interpret `x` as a builtin value.
  ///
  /// - Precondition: `x` is builtin value.
  func asBuiltinValue(_ x: Operand) -> BuiltinValue {
    switch x {
    case .register(let instruction):
      topOfStack.registers[instruction]!.payload as! BuiltinValue
    case .parameter:
      preconditionFailure("Parameter operand doesn't have builtin value.")
    case .constant(let c):
      switch c {
      case let x as IntegerConstant:
        BuiltinValue(x)
      default:
        UNIMPLEMENTED("non-integer constant parsing!!!")
      }
    }
  }

  /// Returns pointer to first instruction of `b`.
  func entryPoint(of b: Block.ID) -> CodePointer {
    let m = programCounter.module
    let f = programCounter.functionInModule
    let i = program.modules[m]![f].firstInstruction(in: b)!
    return .init(module: m, functionInModule: f, instructionInFunction: i)
  }

}

/// An indication of malformed IR.
struct IRError: Error {}
