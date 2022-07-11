/// A return instruction.
public struct ReturnInst: Inst {

  /// The returned value.
  public let value: Operand

  init(value: Operand = .constant(.unit)) {
    self.value = value
  }

  public var type: LoweredType { .object(.unit) }

  public var operands: [Operand] { [value] }

}
