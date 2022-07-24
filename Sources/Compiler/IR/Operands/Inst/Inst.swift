/// A Val IR instruction.
public protocol Inst {

  /// The types of the instruction's results (empty for instructions with no result).
  var types: [LoweredType] { get }

  /// The operands of the instruction.
  var operands: [Operand] { get }

  /// The source range of the code corresponding to that instruction, if any.
  var range: SourceRange? { get }

  /// Indicates whether the instruction is a terminator.
  ///
  /// A "terminator" is an instruction that indicates which block should be executed after the
  /// current block is finished, returns a value, or yields control.
  var isTerminator: Bool { get }

  /// Returns whether the instruction is well-formed.
  func check(in module: Module) -> Bool

}

/// The ID of a Val IR instruction.
public struct InstID: Hashable {

  /// The ID of the containing function.
  public var function: Module.FunctionIndex

  /// The address of the containing block.
  public var block: Function.BlockAddress

  /// The address of the instruction in the containing block.
  public var address: Block.InstAddress

}
