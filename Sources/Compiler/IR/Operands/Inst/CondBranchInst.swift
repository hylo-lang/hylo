/// Branches conditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct CondBranchInst: Inst {

  /// A Boolean condition.
  public let condition: Operand

  /// The target of the branch if `condition` is true.
  public let targetIfTrue: Block.ID

  /// The target of the branch if `condition` is false.
  public let targetIfFalse: Block.ID

  public var type: LoweredType { .object(.unit) }

  public var operands: [Operand] { [] }

}
