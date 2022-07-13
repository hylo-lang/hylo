/// A load instruction.
public struct LoadInst: Inst {

  public var type: LoweredType

  /// The address of the object to load.
  public var source: Operand

  public var range: SourceRange?

  public var operands: [Operand] { [source] }

}
