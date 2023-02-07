import Core

/// An instruction whose semantics is defined by LLVM.
public struct LLVMInstruction: Instruction {

  /// The built-in function representing this instruction.
  public let function: BuiltinFunction

  /// The operands of the instruction.
  public let operands: [Operand]

  public let site: SourceRange

  init(applying f: BuiltinFunction, to operands: [Operand], at site: SourceRange) {
    self.function = f
    self.operands = operands
    self.site = site
  }

  public var types: [LoweredType] { [.object(function.type.output)] }

  public var isTerminator: Bool { false }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
