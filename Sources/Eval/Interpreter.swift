import VIL

/// A VIL code interpreter.
public struct Interpreter {

  /// The call frames of the interpreter.
  var frames: [[Any]] = []

  /// The program counter of the interpreter.
  var pc = ProgramCounter()

  public init() {}

  /// Evaluates the given module.
  public mutating func eval(module: Module) throws {
    // Set the interpreter's program counter at the start of the main function.
    guard let entry = module.functions["main"]?.blocks.first else {
      throw RuntimeError(message: "no entry point")
    }
    pc = ProgramCounter(atStartOf: entry)

    while let nextPC = try step() {
      pc = nextPC
    }
  }

  /// Executes the instruction pointed by the program counter.
  mutating func step() throws -> ProgramCounter? {
    return nil
  }

}
