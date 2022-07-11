/// A Val IR instruction.
public protocol Inst {

  /// The type of the instruction.
  var type: LoweredType { get }

  /// The operands of the instruction.
  var operands: [Operand] { get }

}

/// The ID of a Val IR instruction.
public struct InstID: Hashable {

  /// The ID of the containing function.
  public var function: Module.FunctionIndex

  /// The ID of the containing block.
  public var block: Function.BlockAddress

  /// The address of the instruction in the containing block.
  public var address: Block.InstAddress

}
