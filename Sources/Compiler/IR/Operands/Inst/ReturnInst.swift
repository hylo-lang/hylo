/// A return instruction.
public struct ReturnInst: Inst {

  /// The returned value.
  public var value: Operand

  public var range: SourceRange?

  init(value: Operand = .constant(.unit), range: SourceRange? = nil) {
    self.value = value
    self.range = range
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [value] }

  public func check(in module: Module) -> Bool {
    true
  }

}
