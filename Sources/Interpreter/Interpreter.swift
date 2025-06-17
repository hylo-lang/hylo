import FrontEnd
import Collections
import IR

struct CodePointer {

  var module: Module.ID
  var instructionInModule: InstructionID

}

struct Register {

  let type: IR.`Type`
  let value: Any

}

struct StackFrame {

  var registers: Array<Register>
  var programCounter: CodePointer

}

/// A virtual machine that executes Hylo's in-memory IR representation.
struct Interpreter {

  /// The program to be executed.
  private let program: IR.Program

  /// An instance executing `p`.
  public init(_ p: IR.Program) {
    program = p
  }

  /// Executes a single instruction.
  public func step() throws {}

}
