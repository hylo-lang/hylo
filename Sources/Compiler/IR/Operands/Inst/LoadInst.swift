/// A load instruction.
public struct LoadInst: Inst {

  public let type: LoweredType

  /// The address of the object to load.
  public let source: Operand

  public var operands: [Operand] { [source] }

}
