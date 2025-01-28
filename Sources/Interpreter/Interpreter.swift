import IR

struct CodePointer {

  var module: Module.ID
  var instructionInModule: InstructionID

}

struct Register {
  let type: Type
}

struct StackFrame {

}

/// A virtual machine that executes Hylo's in-memory IR representation.
struct Interpreter {

  /// The program to be executed.
  private let program: Program

  /// An instance executing `p`.
  public init(_ p: Program) {
    program = p
  }

  /// Executes a single instruction.
  public func step() throws {}

}
