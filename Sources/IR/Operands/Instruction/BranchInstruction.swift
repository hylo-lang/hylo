import Core

/// Branches unconditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct BranchInstruction: Instruction {

  /// The target of the branch.
  public let target: Block.ID

  public let site: SourceRange

  init(target: Block.ID, site: SourceRange) {
    self.target = target
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { true }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
