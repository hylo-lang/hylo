/// Branches conditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct CondBranchInst: Inst {

  /// A Boolean condition.
  public var condition: Operand

  /// The target of the branch if `condition` is true.
  public var targetIfTrue: Block.ID

  /// The target of the branch if `condition` is false.
  public var targetIfFalse: Block.ID

  public var range: SourceRange?

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [condition] }

  public var isTerminator: Bool { true }

  public func check(in module: Module) -> Bool {
    /// The condition operand has an object type.
    return !module.type(of: condition).isAddress
  }

}
