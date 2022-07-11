/// Branches unconditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct BranchInst: Inst {

  /// The target of the branch.
  public let target: Block.ID

  public var type: LoweredType { .object(.unit) }

  public var operands: [Operand] { [] }

}
