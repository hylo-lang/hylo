/// A return instruction.
public struct ReturnInst: Inst {

  /// The returned value.
  public let value: Operand

  init(value: Operand = .constant(.unit)) {
    self.value = value
  }

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("return ")
    value.dump(into: &output, with: &printer)
  }

  public var type: LoweredType { .object(.unit) }

  public var operands: [Operand] { [value] }

}
