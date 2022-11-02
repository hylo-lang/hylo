/// A return instruction.
public struct ReturnInst: Inst {

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

  public func check(in module: Module) -> Bool {
    true
  }

}
