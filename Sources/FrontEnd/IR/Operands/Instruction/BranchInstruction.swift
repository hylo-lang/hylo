import Core

/// Branches unconditionally to the start of a basic block.
///
/// `target` must be in the same function.
public struct BranchInstruction: Instruction {

  /// The target of the branch.
  public let target: Block.ID

  public let range: SourceRange?

  init(target: Block.ID, range: SourceRange? = nil) {
    self.target = target
    self.range = range
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [] }

  public var isTerminator: Bool { true }

  public func isWellFormed(in module: Module) -> Bool {
    true
  }

}
