import Core

/// A return instruction.
public struct ReturnInstruction: Instruction {

  /// The returned value.
  public let value: Operand

  public let range: SourceRange?

  init(value: Operand = .constant(.void), range: SourceRange? = nil) {
    self.value = value
    self.range = range
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [value] }

  public var isTerminator: Bool { true }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
