/// A Val IR instruction.
public protocol Inst {

  /// The type of the instruction.
  var type: LoweredType { get }

  /// Writes the textual representation of the instruction into `output` using `printer` to
  /// pretty-print its operands.
  func dump<Target: TextOutputStream>(into output: inout Target, with printer: inout IRPrinter)

}

/// The ID of a Val IR instruction.
public struct InstID: Hashable {

  /// The ID of the containing function.
  public var function: Module.FunctionIndex

  /// The ID of the containing block.
  public var block: Function.BlockIndex

  /// The index of the instruction in the containing block.
  public var index: Block.InstIndex

}
