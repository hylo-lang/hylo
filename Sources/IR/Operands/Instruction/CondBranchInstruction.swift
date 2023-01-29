import Core

/// Branches conditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct CondBranchInstruction: Instruction {

  /// A Boolean condition.
  public let condition: Operand

  /// The target of the branch if `condition` is true.
  public let targetIfTrue: Block.ID

  /// The target of the branch if `condition` is false.
  public let targetIfFalse: Block.ID

  public let site: SourceRange

  init(condition: Operand, targetIfTrue: Block.ID, targetIfFalse: Block.ID, site: SourceRange) {
    self.condition = condition
    self.targetIfTrue = targetIfTrue
    self.targetIfFalse = targetIfFalse
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [condition] }

  public var isTerminator: Bool { true }

  public func isWellFormed(in module: Module) -> Bool {
    /// The condition operand has an object type.
    return !module.type(of: condition).isAddress
  }

}
