/// A return instruction.
public struct ReturnInst: Inst {

  /// The returned value.
  public let value: Operand?

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    if let value = value {
      output.write("return ")
      value.dump(into: &output, with: &printer)
    } else {
      output.write("return")
    }
  }

  public var type: LoweredType { .object(.unit) }

}
