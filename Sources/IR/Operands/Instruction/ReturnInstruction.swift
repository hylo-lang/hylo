import Core

/// A return instruction.
public struct ReturnInstruction: Instruction {

  /// The returned value.
  public let value: Operand

  public let site: SourceRange

  init(value: Operand = .constant(.void), site: SourceRange) {
    self.value = value
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [value] }

  public var isTerminator: Bool { true }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
