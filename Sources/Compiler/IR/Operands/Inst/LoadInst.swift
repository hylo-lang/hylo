/// A load instruction.
public struct LoadInst: Inst {

  /// The address of the object to load.
  public let source: Operand

  public let type: LoweredType

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("load ")
    source.dump(into: &output, with: &printer)
  }

  public var operands: [Operand] { [source] }

}
