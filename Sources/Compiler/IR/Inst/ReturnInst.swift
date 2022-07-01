/// A return instruction.
public struct ReturnInst: Inst {

  /// The returned value.
  public let value: Operand

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("return ")
    value.dump(into: &output, with: &printer)
  }

  public var type: IRType { .owned(.unit) }

}
