import Core

/// Branches unconditionally to the start of a basic block.
public struct BranchInstruction: Terminator {

  /// The target of the branch.
  public private(set) var target: Block.ID

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(target: Block.ID, site: SourceRange) {
    self.target = target
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [] }

  public var successors: [Block.ID] { [target] }

  mutating func replaceSuccessor(_ old: Block.ID, _ new: Block.ID) -> Bool {
    precondition(new.function == target.function)
    if target == old {
      target = new
      return true
    } else {
      return false
    }
  }

}

extension Module {

  /// Creates a `branch` anchored at `anchor` that unconditionally jumps at the start of a block.
  ///
  /// - Parameters:
  ///   - target: The block in which control flow jumps.
  func makeBranch(
    to target: Block.ID,
    anchoredAt anchor: SourceRange
  ) -> BranchInstruction {
    return BranchInstruction(target: target, site: anchor)
  }

}
